{-# LANGUAGE RecordWildCards #-}

module FileDirectory where

import System.Directory(listDirectory, doesDirectoryExist)
import Control.Monad.Cont (forM_)
import System.FilePath.Posix
import Data.List (isPrefixOf)
import Control.Monad.State (StateT)
import Control.Monad.Except (Except)
import qualified Data.ByteString.Char8 as BS

type TreeMonad = (StateT (FilesTree, FilePath) (Except String))

data FilesTree = File { path :: FilePath
                      , fileData :: BS.ByteString }
               | Dir { children  :: [FilesTree]
                     , path :: FilePath } deriving (Eq, Show)

isDir :: FilesTree -> Bool
isDir Dir{..} = True
isDir File{..} = False

isFile :: FilesTree -> Bool
isFile = not . isDir

getTreeName :: FilesTree -> FilePath
getTreeName Dir{..} = takeBaseName path
getTreeName File{..} = takeFileName path

getCurrentTree :: FilesTree -> FilePath -> FilesTree
getCurrentTree curTree@Dir{..} fPath
  | path == fPath = curTree
  | otherwise = getNext fPath children

addToTree ::  (FilesTree, FilePath) -> FilesTree -> FilesTree
addToTree (curTree, curDir) file
  | path curTree == curDir = curTree {children = children curTree ++ [file]}
  | isDir curTree && path curTree `isPrefixOf` curDir = curTree {
      children = map (\ child -> addToTree (child, curDir) file) (children curTree)
    }
  | otherwise = curTree

removeFromTree :: (FilesTree, FilePath) -> FilePath -> FilesTree
removeFromTree (curTree, curDir) file
  | path curTree == curDir = curTree { children = removeChild file (children curTree) }
  | isDir curTree && path curTree `isPrefixOf` curDir = curTree {
      children = map (\child -> removeFromTree (child, curDir) file) (children curTree)
    }
  | otherwise = curTree
  where
    removeChild :: FilePath -> [FilesTree] -> [FilesTree]
    removeChild _ [] = []
    removeChild x (y:ys) | x == path y = removeChild x ys
                        | otherwise = y : removeChild x ys

modifyFile :: (FilesTree, FilePath) -> FilePath -> String -> FilesTree
modifyFile (curTree, curDir) file text
  | path curTree == file = curTree { fileData = addStrToBS text (fileData curTree) }
  | isDir curTree && path curTree `isPrefixOf` curDir = curTree {
      children = map (\child -> modifyFile (child, curDir) file text) (children curTree)
    }
  | otherwise = curTree


addStrToBS :: String -> BS.ByteString -> BS.ByteString
addStrToBS addText bs = BS.append bs (BS.pack addText)

getNext :: FilePath -> [FilesTree] -> FilesTree
getNext fPath (next:xs) = if isDir next && isPrefixOf (path next) fPath
                          then
                            getCurrentTree next fPath
                          else
                            getNext fPath xs

hasNext :: FilePath -> [FilesTree] -> Maybe FilesTree
hasNext _ [] = Nothing
hasNext fPath (tr:xs) = if path tr == fPath
                        then Just tr
                        else hasNext fPath xs

findFileWithRes :: FilesTree -> FilePath -> BS.ByteString
findFileWithRes File{} _ = BS.empty
findFileWithRes curTree@Dir{} name = mapper (children curTree)
  where
    mapper :: [FilesTree] -> BS.ByteString
    mapper [] = BS.empty
    mapper (child:lastC) = 
      do
        let bs = mapper lastC
        if isDir child then
            BS.append bs (findFileWithRes child name)
        else
            if path curTree ++ "/" ++ name == path child then
                addStrToBS (path child  ++ "\n") bs
            else
                bs
                                                  

printFT :: Int -> FilesTree -> IO()
printFT cnt File{..}  = do
  putStrLn $ duplicate "-" cnt ++ ">" ++ path
  BS.putStrLn fileData
printFT cnt Dir{..}  = do
  putStrLn $ duplicate "-" cnt ++ ">" ++ path
  forM_ children (printFT (cnt + 1))

duplicate :: String -> Int -> String
duplicate string n = Prelude.concat $ replicate n string

rootDir :: FilePath
rootDir = "/home/damm1t/Workspace/TestFolder"

readFS :: FilePath -> IO FilesTree
readFS curPath = do
  isDirExist <- doesDirectoryExist curPath
  if isDirExist then do
    list <- readDir curPath
    return $ Dir list curPath
  else do
    contents <- BS.readFile curPath
    return $ File curPath (BS.length contents `seq` contents)

readDir :: FilePath -> IO [FilesTree]
readDir curPath = do listDirs <- listDirectory curPath
                     traverse (readFS . (\ name -> curPath ++ "/" ++ name)) listDirs


testFunc :: IO ()
testFunc = do
  fs <- readFS rootDir
  printFT 0 fs