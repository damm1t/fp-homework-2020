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
import PrintFileSystem

commandsParser :: (FilesTree, FilePath) -> IO ()
commandsParser ini = do
  putStr $ snd ini ++ " > "
  hFlush stdout
  input <- getLine
  (command:args) <- getArgs input
  case command of
    "exit" -> exit ini
    "cd" -> makeCD ini [unwords args]
    
    "ls" -> makeLS ini [unwords args]
    "cat" -> openFile ini args
    "fail" -> commandsParser ini
    "create-file" -> createFile ini args
    "find-file" -> findFile ini args
    "create-folder" -> createFolder ini [unwords args]
    "remove" -> removeElement ini [unwords args]
    "information" -> comandShowInfo ini [unwords args]
    "cvs-init" -> initCVS ini
    "cvs-add" -> addFileToCVS ini [unwords args]
    "cvs-cat" ->
      do
         let (fileName:other) = args
         let suffixArgs = BS.pack $ unwords other
         if BS.length suffixArgs > 1 && BS.head suffixArgs == '"' && BS.last suffixArgs == '"' then
           do
             let nameVersion = BS.unpack $ BS.tail $ BS.init suffixArgs
             openCVSFile ini (fileName : [nameVersion])
         else openCVSFile ini [fileName]
    "cvs-update" ->
      do
        let (fileName:other) = args
        let suffixArgs = BS.pack $ unwords other
        if BS.length suffixArgs > 1 && BS.head suffixArgs == '"' && BS.last suffixArgs == '"' then
          do
            let nameVersion = BS.unpack $ BS.tail $ BS.init suffixArgs
            addVersion ini (fileName : [nameVersion])
        else addVersion ini [fileName]
    "cvs-remove" -> removeFileCVS ini [unwords args]
    "dir" ->
      do
        DT.tui ini
        commandsParser ini
    "write-file" ->
      do
        let (fileName:other) = args
        let suffixArgs = BS.pack $ unwords other
        if BS.length suffixArgs > 1 && BS.head suffixArgs == '"' && BS.last suffixArgs == '"' then
            do
              let text = BS.unpack $ BS.tail $ BS.init suffixArgs
              writeToFile ini (fileName : [text])
        else writeToFile ini [fileName]
    "help" -> do
         putStrLn helpMsg
         commandsParser ini
    _ ->
      do
        putStrLn "error"
        commandsParser ini

getArgs :: String -> IO [String]
getArgs "" = do
  putStrLn "Please, enter command or use help"
  return ["fail"]
getArgs s = return $ words s


helpMsg :: String
helpMsg = "cd <folder> -- перейти в директорию \n"
           ++ "dir -- показать содержимое текущей директории\n"
           ++ "ls <folder> -- показать содержимое выбранной директории\n"
           ++ "create-folder \"folder-name\" -- создать директорию в текущей\n"
           ++ "cat <file> -- показать содержимое файла\n"
           ++ "create-file \"file-name\" -- создать пустой файл в текущей директории\n"
           ++ "remove <folder | file> -- удалить выборанную директорию или файл\n"
           ++ "write-file <file> \"text\" -- записать текст в файл\n"
           ++ "find-file \"file-name\" --  поиск файла в текущией директории и поддиректориях\n"
           ++ "information <file> -- показать информацию о файле\n"
           ++ "information <folder> -- показать информацию о директории\n"
           ++ "cvs-init -- инициализация СКВ в текущей выбранной директории\n"
           ++ "cvs-add <file | folder> -- добавление файла или папки в СКВ\n"
           ++ "cvs-update <file> \"comment\" -- добавление изменений файла в СКВ\n"
           ++ "cvs-history <file> -- просмотр истории изменений файла\n"
           ++ "cvs-cat <file> \"index\" -- просмотр конкретной ревизии файла\n"
           ++ "cvs-merge-revs <file> \"index1\" \"index2\" \"left | right | both | interactive\" --\n"
           ++ "объединение ревизий файла по заданным индексам, left, right, both или interactive\n"
           ++ "являются вариантами стратегий для обеъединения\n"
           ++ "cvs-delete-version <file> \"index\" -- удалить заданную версию файла из ревизий\n"
           ++ "cvs-remove <file> -- удалить файл из СКВ\n"
           ++ "cvs-show-everything -- показать общую историю изменений\n"
           ++ "help --  показать руководство по использованию\n"
           ++ "exit -- завершение работы программы\n"

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

openCVSFile :: (FilesTree, FilePath) -> [String] -> IO ()
openCVSFile ini args = do
  let res = FP.parseWithText args
  case getParseResult res of
   Just FP.ParamsFile{..} ->
     case runExcept (runStateT (FP.execOpenVersion fileName addText) ini) of
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

addVersion :: (FilesTree, FilePath) -> [String] -> IO ()
addVersion ini args = do
  let res = FP.parseWithText args
  case getParseResult res of
   Just FP.ParamsFile{..} ->
     case runExcept (runStateT (FP.execAddVersion fileName addText) ini) of
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

initCVS :: (FilesTree, FilePath) -> IO ()
initCVS ini = case runExcept (runStateT (FP.execInitCVS "cvs") ini) of
                     Right pr -> commandsParser $ snd pr
                     Left msg
                       -> do putStrLn msg
                             commandsParser ini


addFileToCVS :: (FilesTree, FilePath) -> [String] -> IO ()
addFileToCVS ini args = do
  let res = FP.parse "Tree" args
  case getParseResult res of
   Just FP.Params{..} ->
     case runExcept (runStateT (FP.execAddCVS name) ini) of
       Right pr -> commandsParser $ snd pr
       Left msg ->
         do
           putStrLn msg
           commandsParser ini
   Nothing ->
     do
       FP.printFail res
       commandsParser ini

removeFileCVS :: (FilesTree, FilePath) -> [String] -> IO ()
removeFileCVS ini args = do
 let res = FP.parse "File" args
 case getParseResult res of
  Just FP.Params{..} ->
    case runExcept (runStateT (FP.execRemoveCVS name) ini) of
      Right pr -> commandsParser $ snd pr
      Left msg ->
        do
          putStrLn msg
          commandsParser ini
  Nothing ->
    do
      FP.printFail res
      commandsParser ini
