{-# LANGUAGE RecordWildCards #-}
module DirParser where

import FileDirectory
import Options.Applicative
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import System.FilePath (takeDirectory)

newtype Params = Params {folder :: String}

parse :: [String] -> ParserResult Params
parse = execParserPure defaultPrefs opts
  where
    parser = Params <$> argument str (metavar "FOLDER")
    opts = info parser mempty


-- cd
execCommand :: String -> TreeMonad ()
execCommand val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair

  case val of
    ".." ->
      if curPath == path tree then
          throwError "Can't go up from current dir"
      else
          modify (\ (x, _) -> (x, takeDirectory curPath))

    "." ->
      modify (\ (x, _) -> (x, path tree))
    _ ->
      do
        let curTree = getCurrentTree tree curPath
        case hasNext (curPath ++ "/" ++ val) (children curTree) of
          Just Dir{..} -> modify (\(x, y) -> (x, y ++ "/" ++ val))
          Just File{..} -> throwError $ "File -> " ++ val ++ " <- is not a diretory"
          _ -> throwError $ "-> " ++ val ++ " <- Dirictory not exist"


printFail :: ParserResult a -> IO()
printFail (Failure failure) = do
  let (msg, exit) = renderFailure failure ""
  putStrLn msg
printFail _ = undefined 