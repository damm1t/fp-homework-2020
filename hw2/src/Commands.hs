{-# LANGUAGE RecordWildCards #-}
module Commands where
import FileDirectory
import Control.Monad.State
import Control.Monad.Except
import Data.List.Split
import System.Directory (doesDirectoryExist)
import Data.List (isPrefixOf)
import System.IO (hFlush, stdout)

type TreeMonad = (StateT (FilesTree, FilePath) (Except String))

commandsParser :: (FilesTree, FilePath) -> IO ()
commandsParser ini = do
  putStr $ snd ini ++ "> "
  hFlush stdout
  input <- getLine
  let arrInput = words input
  let command = head arrInput
  if command == "exit" then
      putStrLn "stopping"
  else do
    let valDir = arrInput !! 1
    case runExcept (runStateT (execCommand valDir) ini) of
      Right pr -> commandsParser $ snd pr
      Left msg -> do
        print msg
        commandsParser ini



{-getDirList :: FilePath -> FilePath -> [FilePath]
getDirList "" ('/':list) = getDirList "" list
getDirList "" dirs = splitOn "/" dirs
getDirList (x:xs) (y:ys) = getDirList xs ys-}

getCurrentTree :: FilesTree -> FilePath -> FilesTree
getCurrentTree curTree@Dir{..} fPath
  | path == fPath = curTree
  | otherwise = getNext fPath children

getNext :: FilePath -> [FilesTree] -> FilesTree
getNext fPath (next:xs) = if isDir next && isPrefixOf (path next) fPath
                          then
                            getCurrentTree next fPath
                          else
                            getNext fPath xs

hasNext :: FilePath -> [FilesTree] -> Bool
hasNext fPath
  = foldr
      (\ next -> (||) (isDir next && isPrefixOf (path next) fPath))
      False

execCommand :: String -> TreeMonad ()
execCommand val = do
  pair <- get
  let tree = fst pair
  let path = snd pair
  let curTree = getCurrentTree tree path
  let isExist = hasNext (path ++ "/" ++ val) (children curTree)
  if isExist then
    modify (\(x, y) -> (x, y ++ "/" ++ val))
  else
    throwError $ "-> " ++ val ++ " <-Not a dirictory"
