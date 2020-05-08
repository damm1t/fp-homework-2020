{-# LANGUAGE RecordWildCards #-}
module PrintFileSystem where

import qualified Data.ByteString.Char8 as BS
import FileDirectory
import System.Directory (removeDirectoryRecursive, createDirectory, setPermissions)

exit :: (FilesTree, FilePath) -> IO ()
exit (curTree, _) = do
  removeDirectoryRecursive (path curTree)
  generateNewFS curTree

generateNewFS :: FilesTree -> IO ()
generateNewFS Dir{..} =
  do
    createDirectory path
    setPermissions path (dirPerm dirInfo)
    myMap children
generateNewFS File{..} =
  do
    BS.writeFile path fileData
    setPermissions path (filePerm fileInfo)
generateNewFS CVS{..} =
  do
    createDirectory path
    printListCVS versionsInfo

printListCVS :: [CVSInfo] -> IO()
printListCVS [] = return ()
printListCVS (x:xs) =
  do
    printCVS x
    printListCVS xs

printCVS :: CVSInfo -> IO()
printCVS CVSInfo{..} = compareList listFiles curPath
  where
  compareList :: [(String, BS.ByteString)] -> FilePath -> IO()
  compareList [] _ = return ()
  compareList ((st, bs):xs) fPath = 
    do
      BS.writeFile (fPath ++ "#" ++ st) bs
      compareList xs fPath

myMap :: [FilesTree] -> IO ()
myMap [] = return ()
myMap (x:xs) =
  do
    generateNewFS x
    myMap xs