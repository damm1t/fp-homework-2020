{-# LANGUAGE RecordWildCards #-}

module FileDirectory where

import System.Directory(Permissions)
import Control.Monad.Cont (forM_)
import System.FilePath.Posix
import Data.List (isPrefixOf)
import Control.Monad.State (StateT)
import Control.Monad.Except (Except)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Utils

type TreeMonad = (StateT (FilesTree, FilePath) (Except String))

data FilesTree 
  -- | 'File' constructor implement file type object
  = File { -- | 'path' field stores full path to 'File'
           path :: FilePath,
           -- | 'fileData' stores text of 'File' in 'ByteString'
           fileData :: BS.ByteString,
           -- | 'fileInfo' stores 'File' information
           fileInfo :: FileInfo
         }
  -- | 'Dir' constructor implement folder type object
  | Dir { -- | 'path' field stores full path to 'Dir'
          path :: FilePath,
          -- | 'children' field stores 'Dir' content
          children  :: [FilesTree],
          -- | 'dirInfo' field stores 'Dir' information
          dirInfo :: DirInfo
        } 
  -- | 'CVS' constructor implement cvs file type object
  | CVS { -- | 'path' field stores full path to 'CVS'
          path :: FilePath,
          -- | versionsInfo field stores 'CVS' file content
          versionsInfo :: [CVSInfo]
        } deriving (Eq, Show)
                     
data CVSInfo 
  -- | 'CVSInfo' constructor implement cvs file information
  = CVSInfo { -- | 'curPath' field stores full path to file at 'CVSInfo'
              curPath :: FilePath,
              -- | 'listFiles' field stores list of versions file
              listFiles :: [(String, BS.ByteString)]
            } deriving (Eq, Show)   

data FileInfo 
  -- | 'FileInfo' constructor implement file information
  = FileInfo { -- | 'filePath' field stores full path to file
               filePath :: FilePath,
               -- | 'filePerm' field stores full permissions of file
               filePerm :: Permissions,
               -- | 'ext' field stores type of file
               ext :: String,
               -- | 'fileTime' field stores file's last modification time
               fileTime :: String,
               -- | 'fileSize' field stores file size in Bytes
               fileSize :: Int
             } deriving (Eq, Show)

data DirInfo
  -- | 'DirInfo' constructor implement folder information
  = DirInfo { -- | 'dirPath' field stores full path to folder
              dirPath :: FilePath,
              -- | 'dirPerm' field stores full permissions of folder
              dirPerm :: Permissions,
              -- | 'dirSize' field stores folder size in Bytes
              dirSize :: Int,
              -- | 'dirCount' field stores count files at folder
              dirCount :: Int
            } deriving (Eq, Show)

-- | 'updateFileInfo' function update input 'FileInfo' with new time and text      
updateFileInfo :: FileInfo -> UTCTime -> BS.ByteString -> FileInfo
updateFileInfo oldInfo curTime newText = oldInfo { fileTime = show curTime
                                                 , fileSize = BS.length newText
                                                 }

-- | 'getCountFiles' function returns count of files in folder content
getCountFiles :: [FilesTree] -> Int
getCountFiles = foldr (\ tree res -> getFilesInside tree + res) 0
  where
    getFilesInside :: FilesTree -> Int
    getFilesInside File{..} = 1
    getFilesInside Dir{..} = getCountFiles children
    getFilesInside _ = 0

-- | 'updateDirInfo' function update folder info with new content
updateDirInfo :: DirInfo -> [FilesTree] -> DirInfo
updateDirInfo oldInfo children = oldInfo { dirSize = getDirSize children
                                         , dirCount = getCountFiles children
                                         }

-- | 'getDirSize' function returns size of folder content
getDirSize :: [FilesTree] -> Int
getDirSize = foldr (\ tree res -> res + getTreeSize tree) 0

-- | 'getTreeSize' function returns size of 'FilesTree'
getTreeSize :: FilesTree -> Int
getTreeSize Dir{..} = getDirSize children
getTreeSize File{..} = fileSize fileInfo
getTreeSize _ = 0

getTreePerm :: FilesTree -> Permissions
getTreePerm Dir{..} = dirPerm dirInfo
getTreePerm File{..} = filePerm fileInfo
getTreePerm _ = undefined

isDir :: FilesTree -> Bool
isDir Dir{..} = True
isDir _ = False

isFile :: FilesTree -> Bool
isFile File{..} = True
isFile _ = False

isTree :: FilesTree -> Bool
isTree CVS{..} = False
isTree _ = True

isCVS :: FilesTree -> Bool
isCVS = not . isTree

getTreeName :: FilesTree -> FilePath
getTreeName Dir{..} = takeBaseName path
getTreeName File{..} = takeFileName path
getTreeName CVS{..} = "cvs"

getCurrentTree :: FilesTree -> FilePath -> FilesTree
getCurrentTree curTree@Dir{..} fPath
  | path == fPath = curTree
  | otherwise = getNext fPath children
getCurrentTree _ _ = undefined

-- executable tree functions

-- | 'addToTree' function adds new 'FilesTree' to file system
addToTree :: (FilesTree, FilePath) -> FilesTree -> FilesTree
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
  
-- | 'addCVS' function initialize new cvs 'FilesTree' to file system
addCVS :: (FilesTree, FilePath) -> FilesTree -> FilesTree
addCVS  (curTree, curDir) cvs 
  | path curTree == curDir =
    do
      let newChildren = children curTree ++ [cvs]
      curTree { children = newChildren }
  | isDir curTree && path curTree `isPrefixOf` curDir = 
    do
      let newChildren = map (\ child -> addCVS (child, curDir) cvs) (children curTree)
      curTree { children = newChildren }
  | otherwise = curTree

-- | 'removeFromTree' function remove 'FilesTree' from file system
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
         
-- | 'addCVSFile' function add new cvs 'FilesTree' to file system               
addCVSFile :: (FilesTree, FilePath) -> FilePath -> FilesTree
addCVSFile (curTree, curDir) file
  | path curTree == curDir = 
    do
      let curFileData = getFileData file (children curTree)
      let newChildren = addFile curFileData file (children curTree)
      curTree { children = newChildren }
  | isDir curTree && path curTree `isPrefixOf` curDir = 
    do
      let newChildren = map (\child -> addCVSFile (child, curDir) file) (children curTree)
      curTree { children = newChildren }
  | otherwise = curTree
  where
    addFile :: BS.ByteString -> FilePath -> [FilesTree] -> [FilesTree]
    addFile _ _ [] = []
    addFile curFileData curPath (x:xs)
      | isCVS x = (x {versionsInfo = versionsInfo x
                                       ++
                                         [CVSInfo
                                            {curPath = getCVSFileName x curPath,
                                             listFiles = [("initial", curFileData)]}]})
                    : xs
      | otherwise = x:addFile curFileData curPath xs
    
-- | 'removeCVSFile' function remove cvs 'FilesTree' from file system    
removeCVSFile :: (FilesTree, FilePath) -> FilePath -> FilesTree
removeCVSFile (curTree, curDir) file
  | path curTree == curDir = 
    do
      let newChildren = removeFile file (children curTree)
      curTree { children = newChildren }
  | isDir curTree && path curTree `isPrefixOf` curDir = 
    do
      let newChildren = map (\child -> removeCVSFile (child, curDir) file) (children curTree)
      curTree { children = newChildren }
  | otherwise = curTree
  where
    removeFile :: FilePath -> [FilesTree] -> [FilesTree]
    removeFile _ [] = []
    removeFile fPath (x:xs)
      | isCVS x = (x {versionsInfo = remCVSFile (getCVSFileName x fPath) (versionsInfo x)}): xs
      | otherwise = x:removeFile fPath xs
    remCVSFile :: FilePath -> [CVSInfo] -> [CVSInfo]
    remCVSFile _ [] = []
    remCVSFile rPath (y:ys)
      | curPath y == rPath = ys
      | otherwise = y:remCVSFile rPath ys

-- | 'getFileData' function get file content
getFileData :: FilePath -> [FilesTree] -> BS.ByteString
getFileData _ [] = BS.empty
getFileData curPath (x:xs) 
  | curPath == path x = fileData x
  | otherwise = getFileData curPath xs

-- | 'showTreeInfo' function return 'FilesTree' information
showTreeInfo :: FilesTree -> FilePath -> BS.ByteString
showTreeInfo curTree curPath
  | path curTree == curPath = 
    BS.concat $ map BS.pack (printInfo curTree)
  | isDir curTree && path curTree `isPrefixOf` curPath = 
    BS.concat $ map (`showTreeInfo` curPath) (children curTree) 
  | otherwise = BS.empty
  where
    printInfo :: FilesTree -> [String]
    printInfo Dir{..} = do
        let size = dirSize dirInfo
        [ "Directory path: " ++ dirPath dirInfo ++ "\n"
          , "Directory size: " ++ show size
            ++ " Byte" ++ (if size > 1 then "s\n" else "\n")
          , "Count files: " ++ show (dirCount dirInfo) ++ "\n"
          , show (dirPerm dirInfo) ++ "\n"
          ]
    printInfo File{..} = do
       let size = fileSize fileInfo
       let formatType = if ext fileInfo == "" then
                            "File has no format\n"
                        else
                            "Format type: " ++ ext fileInfo ++ "\n"
       [ "File path: " ++ filePath fileInfo ++ "\n"
         , "File size: " ++ show size 
           ++ " Byte" ++ (if size > 1 then "s\n" else "\n")
         , formatType
         , "File last modified time: " ++ show (fileTime fileInfo) ++ "\n"
         , show (filePerm fileInfo) ++ "\n"
         ]
    printInfo _ = []

-- | 'modifyFile' function modify file with new content in file system
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

findCVSDir :: (FilesTree, FilePath) -> FilePath -> String -> FilesTree
findCVSDir (curTree, curPath) fPath versionName 
  | curPath == path curTree = modifyCVSFile curTree fPath  versionName
  | isDir curTree && path curTree `isPrefixOf` curPath = 
        do
          let newChildren = map (\child -> findCVSDir (child, curPath) fPath versionName) (children curTree)
          curTree { children = newChildren }
  | otherwise = curTree

openCVSVersion :: [FilesTree] -> FilePath -> String -> BS.ByteString
openCVSVersion children rPath versionName = BS.concat $ map (`openVersionInfo` rPath) children
  where
    openVersionInfo :: FilesTree -> FilePath -> BS.ByteString
    openVersionInfo curTree fPath 
      | isCVS curTree = BS.concat $ map (\cur -> openVersion cur (getCVSFileName curTree fPath)) (versionsInfo curTree)
      | otherwise = BS.empty
    openVersion :: CVSInfo -> FilePath -> BS.ByteString
    openVersion cur fPath
      | curPath cur == fPath = BS.concat $ map (\pr -> if fst pr == versionName then snd pr else BS.empty) (listFiles cur)
      | otherwise = BS.empty

modifyCVSFile :: FilesTree -> FilePath -> String -> FilesTree
modifyCVSFile curTree file versionName = 
  do
    let curData = getFileData file (children curTree)
    curTree { children = modifyCVSChildren (versionName, curData) file (children curTree) }

modifyCVSChildren :: (String, BS.ByteString) -> FilePath -> [FilesTree] -> [FilesTree]
modifyCVSChildren _  _ [] = []
modifyCVSChildren version curFilePath (x:xs)
  | isCVS x = (x {versionsInfo = addFileVersion version (getCVSFileName x curFilePath) (versionsInfo x) }):xs
  | otherwise = x : modifyCVSChildren version curFilePath xs
  where
    addFileVersion :: (String, BS.ByteString) -> FilePath -> [CVSInfo] -> [CVSInfo]
    addFileVersion version fPath [] = []
    addFileVersion version fPath (y:ys)
      | curPath y == fPath = (y {listFiles = listFiles y ++ [version]}):ys
      | otherwise = y : addFileVersion version fPath ys

getNext :: FilePath -> [FilesTree] -> FilesTree
getNext fPath (next:xs) = if isDir next && isPrefixOf (path next) fPath
                          then
                            getCurrentTree next fPath
                          else
                            getNext fPath xs
getNext _ [] = undefined

-- | 'hasNext' function returns 'FilesTree' from list with current path
hasNext :: FilePath -> [FilesTree] -> Maybe FilesTree
hasNext _ [] = Nothing
hasNext fPath (tr:xs) = if path tr == fPath
                        then Just tr
                        else hasNext fPath xs

getCVSFileName :: FilesTree -> FilePath -> FilePath
getCVSFileName x curPath = path x ++ "/" ++ takeFileName curPath

hasCVSFile :: FilePath -> [FilesTree] -> Bool
hasCVSFile _ [] = False
hasCVSFile curFilePath (x:xs)
  | isCVS x = hasFileInCVS (getCVSFileName x curFilePath) (versionsInfo x)
  | otherwise = hasCVSFile curFilePath xs
  where
    hasFileInCVS :: FilePath -> [CVSInfo] -> Bool
    hasFileInCVS _ [] = False
    hasFileInCVS fPath (y:ys) 
      |  curPath y == fPath = True
      | otherwise = hasFileInCVS fPath ys
                                

findFileWithRes :: FilesTree -> FilePath -> BS.ByteString
findFileWithRes File{} _ = BS.empty
findFileWithRes CVS{} _ = BS.empty
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
printFT cnt CVS{..}  = putStrLn $ duplicate "-" cnt ++ ">" ++ path ++ show versionsInfo

