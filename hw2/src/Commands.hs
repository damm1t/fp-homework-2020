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
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (getCurrentTime)

commandsParser :: (FilesTree, FilePath) -> IO ()
commandsParser ini = do
  putStr $ snd ini ++ " > "
  hFlush stdout
  input <- getLine
  (command:args) <- getArgs input
  case command of
    "exit" -> putStrLn "stopping"
    "cd" -> makeCD ini [unwords args]
    
    "ls" -> makeLS ini [unwords args]
    "cat" -> openFile ini args
    "fail" -> commandsParser ini
    "create-file" -> createFile ini args
    "find-file" -> findFile ini args
    "create-folder" -> createFolder ini [unwords args]
    "remove" -> removeElement ini [unwords args]
    "information" -> comandShowInfo ini args
    "dir" ->
          do
            DT.tui ini
            commandsParser ini
    "write-file" ->
      do
        let (fileName:last) = args
        let suff = BS.pack $ unwords last
        if BS.length suff > 1 && BS.head suff == '"' && BS.last suff == '"' then
            do
              let text = BS.unpack $ BS.tail $ BS.init suff
              writeToFile ini (fileName : [text])
        else writeToFile ini [fileName]
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
  let res = FP.parse "File" args
  case getParseResult res of
   Just FP.Params{..} ->
     case runExcept (runStateT (FP.execRead name) ini) of
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

createFile :: (FilesTree, FilePath) -> [String] -> IO ()
createFile ini args = do
  let res = FP.parse "File" args
  curTime <- getCurrentTime
  case getParseResult res of
   Just FP.Params{..} ->
     case runExcept (runStateT (FP.execCreateFile curTime name) ini) of
       Right pr -> commandsParser $ snd pr
       Left msg ->
         do
           putStrLn msg
           commandsParser ini
   Nothing ->
     do
       FP.printFail res
       commandsParser ini

createFolder :: (FilesTree, FilePath) -> [String] -> IO ()
createFolder ini args = do
  let res = FP.parse "Dir" args
  case getParseResult res of
   Just FP.Params{..} ->
     case runExcept (runStateT (FP.execCreateFolder name) ini) of
       Right pr -> commandsParser $ snd pr
       Left msg ->
         do
           putStrLn msg
           commandsParser ini
   Nothing ->
     do
       FP.printFail res
       commandsParser ini

removeElement :: (FilesTree, FilePath) -> [String] -> IO ()
removeElement ini args = do
  let res = FP.parse "Tree" args
  case getParseResult res of
   Just FP.Params{..} ->
     case runExcept (runStateT (FP.execRemove name) ini) of
       Right pr -> commandsParser $ snd pr
       Left msg ->
         do
           putStrLn msg
           commandsParser ini
   Nothing ->
     do
       FP.printFail res
       commandsParser ini

comandShowInfo :: (FilesTree, FilePath) -> [String] -> IO ()
comandShowInfo ini args = do
  let res = FP.parse "Tree" args
  case getParseResult res of
   Just FP.Params{..} ->
     case runExcept (runStateT (FP.execShowInfo name) ini) of
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

writeToFile :: (FilesTree, FilePath) -> [String] -> IO ()
writeToFile ini args = do
  curTime <- getCurrentTime
  let res = FP.parseWithText args
  case getParseResult res of
   Just FP.ParamsFile{..} ->
     case runExcept (runStateT (FP.execAddText curTime fileName addText) ini) of
       Right pr -> commandsParser $ snd pr
       Left msg ->
         do
           putStrLn msg
           commandsParser ini
   Nothing ->
     do
       FP.printFail res
       commandsParser ini
       
findFile :: (FilesTree, FilePath) -> [String] -> IO ()
findFile ini args = do
  let res = FP.parse "File" args
  case getParseResult res of
    Just FP.Params{..} ->
      case runExcept (runStateT (FP.execFindFile name) ini) of
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