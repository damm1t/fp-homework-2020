module Commands where
import Control.Monad.State
import Control.Monad.Except
import System.IO (hFlush, stdout)
import Tui
import FileDirectory
import System.FilePath.Posix

type TreeMonad = (StateT (FilesTree, FilePath) (Except String))

commandsParser :: (FilesTree, FilePath) -> IO ()
commandsParser ini = do
  putStr $ snd ini ++ " > "
  hFlush stdout
  input <- getLine
  let arrInput = words input
  let command = head arrInput
  case command of
    "exit" -> putStrLn "stopping"
    "cd" ->
      do
        let valDir = arrInput !! 1
        case runExcept (runStateT (execCommand valDir) ini) of
          Right pr -> commandsParser $ snd pr
          Left msg -> do
            print msg
            commandsParser ini
    "dir" ->
      do
        tui ini
        commandsParser ini
    _ ->
      do
        putStrLn "error"
        commandsParser ini

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
        let isExist = hasNext (curPath ++ "/" ++ val) (children curTree)
        if isExist then
          modify (\(x, y) -> (x, y ++ "/" ++ val))
        else
          throwError $ "-> " ++ val ++ " <-Not a dirictory"

