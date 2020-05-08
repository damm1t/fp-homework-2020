{-# LANGUAGE RecordWildCards #-}
module FileParser where
import FileDirectory
import Options.Applicative
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)

newtype Params = Params {name :: String}

data ParamsFile = ParamsFile { fileName :: String
                             , addText :: String
                             }

parse :: String -> [String] -> ParserResult Params
parse typeTree = execParserPure defaultPrefs opts
  where
    var = case typeTree of
      "Dir" -> "DIRNAME"
      "File" -> "FILENAME"
      _ -> "DIRNAME or FILENAME"

    parser = Params <$> argument str (metavar var)
    opts = info parser mempty

parseWithText :: [String] -> ParserResult ParamsFile
parseWithText = execParserPure defaultPrefs opts
  where
    parser = ParamsFile <$> argument str (metavar "FILENAME")
                        <*> argument str (metavar "TEXT")
    opts = info parser mempty

printFail :: ParserResult a -> IO()
printFail (Failure failure) = do
  let (msg, exit) = renderFailure failure ""
  putStrLn msg

execRead :: String -> TreeMonad BS.ByteString
execRead val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> throwError $ "File -> " ++ val ++ " <-  not exist"
    Just File{..} -> return fileData
    Just Dir{..} ->
      throwError $ "Directory -> " ++ val ++ " <- is not a file"
    Just _ -> throwError $ "Unavailable name -> " ++ val ++ " <-"

execOpenCVSFile :: String -> TreeMonad BS.ByteString
execOpenCVSFile val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> throwError $ "File -> " ++ val ++ " <-  not exist"
    Just File{..} -> return fileData
    Just Dir{..} ->
      throwError $ "Directory -> " ++ val ++ " <- is not a file"
    Just _ -> throwError $ "Unavailable name -> " ++ val ++ " <-"

createFileInfo :: FilePath -> String -> FileInfo
createFileInfo = undefined

execCreateFile :: UTCTime -> String -> TreeMonad ()
execCreateFile curTime val = do 
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let resPath = curPath ++ "/" ++ val
  let curPerm = getTreePerm curTree
  let file
        = File
            { path = resPath, fileData = BS.empty
            , fileInfo = FileInfo
                          { filePath = resPath
                          , filePerm = curPerm
                          , ext = takeExtension resPath
                          , fileTime = curTime
                          , fileSize = 0
                          }
             }
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> modify (\ (x, y) -> (addToTree (x, y) file, y))
    Just File {..}
      -> throwError $ "File -> " ++ val ++ " <-  already exist"
    Just Dir {..}
      -> throwError $ "Directory -> " ++ val ++ " <- already exist"
    Just _
      -> throwError $ "Unavailable name -> " ++ val ++ " <-"

execCreateFolder :: String -> TreeMonad ()
execCreateFolder val = 
  do pair <- get
     let tree = fst pair
     let curPath = snd pair
     let curTree = getCurrentTree tree curPath
     let resPath = curPath ++ "/" ++ val
     let curPerm = getTreePerm curTree
     let dir
           = Dir
               { path = resPath
               , children = []
               , dirInfo = DirInfo
                            { dirPath = resPath
                            , dirPerm = curPerm
                            , dirSize = 0
                            , dirCount = 0
                            }
               }
     case hasNext (curPath ++ "/" ++ val) (children curTree) of
       Nothing -> modify (\ (x, y) -> (addToTree (x, y) dir, y))
       Just File {..}
         -> throwError $ "File -> " ++ val ++ " <-  already exist"
       Just Dir {..}
         -> throwError $ "Directory -> " ++ val ++ " <- already exist"
       Just _
         -> throwError $ "Unavailable name -> " ++ val ++ " <-"

execRemove :: String -> TreeMonad ()
execRemove val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let delElementPath = curPath ++ "/" ++ val
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing ->
      throwError $ "File or directory -> " ++ val ++ " <- not exist"
    Just _ ->
      modify (\(x, y) -> (removeFromTree (x, y) delElementPath, y))
      
execShowInfo :: String -> TreeMonad BS.ByteString
execShowInfo val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let elementPath = curPath ++ "/" ++ val
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing ->
     throwError $ "File or directory -> " ++ val ++ " <- not exist"
    Just _ ->
      return $ showTreeInfo tree elementPath

execFindFile :: String -> TreeMonad BS.ByteString
execFindFile val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let res = findFileWithRes curTree val 
  if res == BS.empty 
  then throwError $ "File -> " ++ val ++ " <- not found"
  else return res


execAddText :: UTCTime -> String -> String -> TreeMonad ()
execAddText curTime val text = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let elemPath = curPath ++ "/" ++ val
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> throwError $ "File -> " ++ val ++ " <- not exist"
    Just File{..} ->
      modify (\(x, y) -> (modifyFile curTime (x, y) elemPath text, y))
    Just Dir{..} ->
      throwError $ "Directory -> "
                   ++ val
                   ++ " <- already exist. Please enter file name"
    Just _ ->
          throwError $ "Unavailable name -> "
                       ++ val ++ " <-"

execAddVersion :: String -> String -> TreeMonad ()
execAddVersion val version = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let elemPath = curPath ++ "/" ++ val
  case hasNext elemPath (children curTree) of
   Nothing -> throwError $ "File -> " ++ val ++ " <- not exist"
   Just File{..} -> if hasCVSFile elemPath (children curTree) then
                      modify (\(x, y) -> (findCVSDir (x, y) elemPath version, y))
                    else throwError $ "File -> " ++ val ++ " <- not added at CVS"

   Just _ ->
         throwError $ "Unavailable File name -> "
                      ++ val ++ " <-"

execOpenVersion :: String -> String -> TreeMonad BS.ByteString
execOpenVersion val version = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let elemPath = curPath ++ "/" ++ val
  if hasCVSFile elemPath (children curTree) then
    return $ openCVSVersion (children curTree) elemPath version
  else throwError $ "File -> " ++ val ++ " <- not added at CVS"


execInitCVS :: String -> TreeMonad ()
execInitCVS val =
  do pair <- get
     let tree = fst pair
     let curPath = snd pair
     let curTree = getCurrentTree tree curPath
     let resPath = curPath ++ "/" ++ val
     let cvs
           = CVS
               { path = resPath
               , versionsInfo = []
               }
     case hasNext (curPath ++ "/" ++ val) (children curTree) of
       Nothing -> modify (\ (x, y) -> (addCVS (x, y) cvs, y))
       Just _
         -> throwError $ "CVS init fail: -> " ++ val ++ " <-  already exist"

execAddCVS :: String -> TreeMonad ()
execAddCVS val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let elemPath = curPath ++ "/" ++ val
  let cvs = curPath ++ "/cvs"
  case hasNext cvs (children curTree) of
    Nothing -> throwError "CVS not initialized"
    Just _ -> case hasNext elemPath (children curTree) of
                Nothing ->
                  throwError $ "File or directory -> " ++ val ++ " <- not exist"
                Just _ ->
                  modify (\(x, y) -> (addCVSFile (x, y) elemPath, y))
                  
execRemoveCVS :: String -> TreeMonad ()
execRemoveCVS val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let elemPath = curPath ++ "/" ++ val
  let cvs = curPath ++ "/cvs"
  case hasNext cvs (children curTree) of
    Nothing -> throwError "CVS not initialized"
    Just _ ->  modify (\(x, y) -> (removeCVSFile (x, y) elemPath, y))