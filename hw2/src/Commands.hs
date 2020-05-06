{-# LANGUAGE RecordWildCards #-}
module Commands where
import Control.Monad.State
import Control.Monad.Except
import System.IO (hFlush, stdout)
import qualified DirTui as DT
import qualified FileTui as FT
import FileDirectory
import qualified DirParser as CD
import qualified FileParser as FP
import Options.Applicative (getParseResult)

commandsParser :: (FilesTree, FilePath) -> IO ()
commandsParser ini = do
  putStr $ snd ini ++ " > "
  hFlush stdout
  input <- getLine
  (command:args) <- getArgs input
  case command of
    "exit" -> putStrLn "stopping"
    "cd" -> makeCD ini args
    "dir" ->
      do
        DT.tui ini
        commandsParser ini
    "ls" -> makeLS ini args
    "cat" -> openFile ini args
    "fail" -> commandsParser ini
    _ ->
      do
        putStrLn "error"
        commandsParser ini

getArgs :: String -> IO [String]
getArgs "" = do
  putStrLn "Please, enter command or use help"
  return ["fail"]
getArgs s = return $ words s

makeCD :: (FilesTree, FilePath) -> [String] -> IO ()
makeCD ini args = do
  let res = CD.parse args
  case getParseResult res of
   Just CD.Params{..} ->
     case runExcept (runStateT (CD.execCommand folder) ini) of
       Right pr -> commandsParser $ snd pr
       Left msg ->
         do
           putStrLn msg
           commandsParser ini
   Nothing ->
     do
       CD.printFail res
       commandsParser ini

makeLS :: (FilesTree, FilePath) -> [String] -> IO ()
makeLS ini args = do
  let res = CD.parse args
  case getParseResult res of
    Just CD.Params{..} ->
      case runExcept (runStateT (CD.execCommand folder) ini) of
        Right pr -> do
          DT.tui $ snd pr
          commandsParser ini
        Left msg ->
          do
            putStrLn msg
            commandsParser ini
    Nothing ->
      do
        CD.printFail res
        commandsParser ini

openFile :: (FilesTree, FilePath) -> [String] -> IO ()
openFile ini args = do
  let res = FP.parse args
  case getParseResult res of
   Just FP.Params{..} ->
     case runExcept (runStateT (FP.execCommand fileName) ini) of
       Right pr -> 
        do
          FT.tui $ fst pr
          commandsParser ini
       Left msg ->
         do
           putStrLn msg
           commandsParser ini
   Nothing ->
     do
       FP.printFail res
       commandsParser ini