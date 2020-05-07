{-# LANGUAGE RecordWildCards #-}

module FileDirectory where

import System.Directory(Permissions, getPermissions, getModificationTime, getFileSize)
import Control.Monad.Cont (forM_)
import System.FilePath.Posix
import Data.List (isPrefixOf)
import Control.Monad.State (StateT)
import Control.Monad.Except (Except)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Utils
type TreeMonad = (StateT (FilesTree, FilePath) (Except String))

-- getPermissions :: FilePath -> IO Permissions
-- setPermissions :: FilePath -> Permissions -> IO ()
-- getModificationTime :: FilePath -> IO UTCTime
-- setModificationTime :: FilePath -> UTCTime -> IO ()
-- getFileSize :: FilePath -> IO Integer
-- takeExtension :: FilePath -> String

data FileInfo = FileInfo { filePath :: FilePath
                         , filePerm :: Permissions
                         , ext :: String
                         , fileTime :: UTCTime
                         , fileSize :: Int
                         } deriving (Eq, Show)

getFileInfo :: FilePath -> BS.ByteString -> IO FileInfo
getFileInfo curPath text = do
  curPerm <- getPermissions curPath
  time <- getModificationTime curPath
  return $ FileInfo
            curPath
            curPerm
            (takeExtension curPath)
            time
            (BS.length text)
            
updateFileInfo :: FileInfo -> UTCTime -> BS.ByteString -> FileInfo
updateFileInfo oldInfo curTime newText = oldInfo { fileTime = curTime
                                                 , fileSize = BS.length newText
                                                 }

data DirInfo = DirInfo { dirPath :: FilePath
                       , dirPerm :: Permissions
                       , dirSize :: Int
                       , dirCount :: Int
                       } deriving (Eq, Show)

getDirInfo :: FilePath  -> [FilesTree] -> IO DirInfo
getDirInfo curPath children = do
  curPerm <- getPermissions curPath
  return $ DirInfo
            curPath
            curPerm
            (getDirSize children)
            (length children)

updateDirInfo :: DirInfo -> [FilesTree] -> DirInfo
updateDirInfo oldInfo children = oldInfo { dirSize = getDirSize children
                                         , dirCount = length children
                                         }
getDirSize :: [FilesTree] -> Int
getDirSize = foldr (\ tree sum -> sum + getTreeSize tree) 0

getTreeSize :: FilesTree -> Int
getTreeSize Dir{..} = getDirSize children
getTreeSize File{..} = fileSize fileInfo

getTreePerm :: FilesTree -> Permissions
getTreePerm Dir{..} = dirPerm dirInfo
getTreePerm File{..} = filePerm fileInfo

data FilesTree = File { path :: FilePath
                      , fileData :: BS.ByteString
                      , fileInfo :: FileInfo
                      }
               | Dir { path :: FilePath
                     , children  :: [FilesTree]
                     , dirInfo :: DirInfo
                     } deriving (Eq, Show)
                     
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
getCurrentTree _ _ = undefined

-- executable tree functions

addToTree ::  (FilesTree, FilePath) -> FilesTree -> FilesTree
addToTree (curTree, curDir) file
  | path curTree == curDir =
    do
      let newChildren = children curTree ++ [file]
      curTree { children = newChildren
              , dirInfo = updateDirInfo (dirInfo curTree) newChildren
              }
  | isDir curTree && path curTree `isPrefixOf` curDir = 
    do
      let newChildren = map (\ child -> addToTree (child, curDir) file) (children curTree)
      curTree { children = newChildren
              , dirInfo = updateDirInfo (dirInfo curTree) newChildren
              }
  | otherwise = curTree

removeFromTree :: (FilesTree, FilePath) -> FilePath -> FilesTree
removeFromTree (curTree, curDir) file
  | path curTree == curDir = 
    do
      let newChildren = removeChild file (children curTree)
      curTree { children = newChildren
              , dirInfo = updateDirInfo (dirInfo curTree) newChildren
              }
  | isDir curTree && path curTree `isPrefixOf` curDir = 
    do
      let newChildren = map (\child -> removeFromTree (child, curDir) file) (children curTree)
      curTree { children = newChildren
              , dirInfo = updateDirInfo (dirInfo curTree) newChildren
              }
  | otherwise = curTree
  where
    removeChild :: FilePath -> [FilesTree] -> [FilesTree]
    removeChild _ [] = []
    removeChild x (y:ys) | x == path y = removeChild x ys
                        | otherwise = y : removeChild x ys

showTreeInfo :: FilesTree -> FilePath -> BS.ByteString
showTreeInfo curTree curPath
  | path curTree == curPath = BS.concat $ map BS.pack (printInfo curTree)
  | isDir curTree && path curTree `isPrefixOf` curPath = BS.concat $ map (`showTreeInfo` curPath) (children curTree) 
  | otherwise = BS.empty
  where
    printInfo :: FilesTree -> [String]
    printInfo Dir{..} = [show dirInfo]
    printInfo File{..} = [show fileInfo]


modifyFile :: UTCTime -> (FilesTree, FilePath) -> FilePath -> String -> FilesTree
modifyFile curTime (curTree, curDir) file text
  | path curTree == file = 
    do
      let newText = addStrToBS text (fileData curTree)
      curTree { fileData = newText
              , fileInfo = updateFileInfo (fileInfo curTree) curTime newText
              }
  | isDir curTree && path curTree `isPrefixOf` curDir = 
    do
      let newChildren = map (\child -> modifyFile curTime (child, curDir) file text) (children curTree)
      curTree { children = newChildren
              , dirInfo = updateDirInfo (dirInfo curTree) newChildren
              }
  | otherwise = curTree

getNext :: FilePath -> [FilesTree] -> FilesTree
getNext fPath (next:xs) = if isDir next && isPrefixOf (path next) fPath
                          then
                            getCurrentTree next fPath
                          else
                            getNext fPath xs
getNext _ [] = undefined

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

