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

parse :: [String] -> ParserResult Params
parse = execParserPure defaultPrefs opts
  where
    parser = Params <$> argument str (metavar "FILENAME")
    opts = info parser mempty


execCommand :: String -> TreeMonad BS.ByteString
execCommand val = do
  pair <- get
  let tree = fst pair
  let curPath = snd pair
  let curTree = getCurrentTree tree curPath
  case hasFile (curPath ++ "/" ++ val) (children curTree) of
    Nothing -> throwError $ "-> " ++ val ++ " <- File not exist"
    Just x@File{..} -> return fileData

printFail :: ParserResult a -> IO()
printFail (Failure failure) = do
  let (msg, exit) = renderFailure failure ""
  putStrLn msg