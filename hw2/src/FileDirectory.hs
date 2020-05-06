{-# LANGUAGE RecordWildCards #-}

module FileDirectory 
  ( FilesTree(..)
  , TreeMonad
  , readFS
  , rootDir
  , isDir
  , printFT
  , getTreeName
  , getCurrentTree
  , getNext
  , hasNext
  , addToTree
  )where

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
  | path curTree == curDir =  Dir (children curTree ++ [file]) (path curTree)
  | isDir curTree && path curTree `isPrefixOf` curDir =
      Dir (map (\child -> addToTree (child, curDir) file) (children curTree)) (path curTree)
  | otherwise = curTree

getNext :: FilePath -> [FilesTree] -> FilesTree
getNext fPath (next:xs) = if isDir next && isPrefixOf (path next) fPath
                          then
                            getCurrentTree next fPath
                          else
                            getNext fPath xs

hasNext :: FilePath -> [FilesTree] -> Maybe FilesTree
hasNext fPath [] = Nothing
hasNext fPath (tr:xs) = if path tr == fPath
                        then Just tr
                        else hasNext fPath xs

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