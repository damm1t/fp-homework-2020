{-# LANGUAGE RecordWildCards #-}
module FileParser where
import FileDirectory
import Options.Applicative
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import qualified Data.ByteString.Char8 as BS

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
    Just Dir{..} -> throwError $ "Directory -> " ++ val ++ " <- is not a file"

execCreateFile :: String -> TreeMonad ()
execCreateFile val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let file = File (curPath ++ "/" ++ val) BS.empty
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> modify (\(x, y) -> (addToTree (x, y) file, y))
    Just File{..} -> throwError $ "File -> " ++ val ++ " <-  already exist"
    Just Dir{..} -> throwError $ "Directory -> " ++ val ++ " <- already exist"

execCreateFolder :: String -> TreeMonad ()
execCreateFolder val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let dir = Dir [] (curPath ++ "/" ++ val)
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> modify (\(x, y) -> (addToTree (x, y) dir, y))
    Just File{..} -> throwError $ "File -> " ++ val ++ " <-  already exist"
    Just Dir{..} -> throwError $ "Directory -> " ++ val ++ " <- already exist"

execRemove :: String -> TreeMonad ()
execRemove val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let delElementPath = curPath ++ "/" ++ val
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> throwError $ "File or directory -> " ++ val ++ " <- not exist"
    Just _ -> modify (\(x, y) -> (removeFromTree (x, y) delElementPath, y))


execAddText :: String -> String -> TreeMonad ()
execAddText val text = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  let elemPath = curPath ++ "/" ++ val
  case hasNext (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> throwError $ "File -> " ++ val ++ " <- not exist"
    Just File{..} -> modify (\(x, y) -> (modifyFile (x, y) elemPath text, y))
    Just Dir{..} -> throwError $ "Directory -> " ++ val ++ " <- already exist. Please enter file name"