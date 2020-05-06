{-# LANGUAGE RecordWildCards #-}
module FileParser where
import FileDirectory
import Options.Applicative
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import Control.Monad.Error (Error, Error)
import Control.Monad.Error.Class (Error)
import qualified Data.ByteString.Char8 as BS

newtype Params = Params {fileName :: String}

parse :: String -> [String] -> ParserResult Params
parse typeTree = execParserPure defaultPrefs opts
  where
    var = if typeTree == "Dir" then "DIRNAME" else "FILENAME" 
    parser = Params <$> argument str (metavar var)
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