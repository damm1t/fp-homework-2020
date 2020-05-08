module ReadFileSystem where

import FileDirectory
import qualified Data.ByteString.Char8 as BS
import System.Directory (doesDirectoryExist, listDirectory, getPermissions, getModificationTime)
import System.FilePath (takeExtension)

rootDir :: FilePath
rootDir = "/home/damm1t/Workspace/TestFolder"

readFS :: FilePath -> IO FilesTree
readFS curPath = do
  isDirExist <- doesDirectoryExist curPath
  if isDirExist then do
    list <- readDir curPath
    curInfo <- getDirInfo curPath list
    return $ Dir
               { path = curPath
               , children = list
               , dirInfo = curInfo
               }
  else do
    contents <- BS.readFile curPath
    curInfo <- getFileInfo curPath contents
    return $ File
               { path = curPath
               , fileData = BS.length contents `seq` contents
               , fileInfo = curInfo
               }

getFileInfo :: FilePath -> BS.ByteString -> IO FileInfo
getFileInfo curPath text = do
  curPerm <- getPermissions curPath
  time <- getModificationTime curPath
  return $ FileInfo
            curPath
            curPerm
            (takeExtension curPath)
            (show time)
            (BS.length text)

getDirInfo :: FilePath  -> [FilesTree] -> IO DirInfo
getDirInfo curPath children = do
  curPerm <- getPermissions curPath
  return $ DirInfo
            curPath
            curPerm
            (getDirSize children)
            (getCountFiles children)

readDir :: FilePath -> IO [FilesTree]
readDir curPath = do listDirs <- listDirectory curPath
                     traverse (readFS . (\ name -> curPath ++ "/" ++ name)) listDirs

testFunc :: IO ()
testFunc = do
  fs <- readFS rootDir
  printFT 0 fs