module ReadFileSystem where

import FileDirectory
import qualified Data.ByteString.Char8 as BS
import System.Directory (doesDirectoryExist, listDirectory)

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

readDir :: FilePath -> IO [FilesTree]
readDir curPath = do listDirs <- listDirectory curPath
                     traverse (readFS . (\ name -> curPath ++ "/" ++ name)) listDirs

testFunc :: IO ()
testFunc = do
  fs <- readFS rootDir
  printFT 0 fs