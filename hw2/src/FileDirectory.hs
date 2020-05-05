{-# LANGUAGE RecordWildCards #-}

module FileDirectory 
  ( FilesTree(..)
  , readFS
  , rootDir
  , isDir
  , printFT
  , getTreeName
  , getCurrentTree
  , getNext
  , hasNext
  )where

import System.Directory(listDirectory, doesDirectoryExist)
import Control.Monad.Cont (forM_)
import System.FilePath.Posix
import Data.List (isPrefixOf)

data FilesTree = File { path :: FilePath }
               | Dir { children  :: [FilesTree]
                     , path :: FilePath } deriving (Eq, Show)

isDir :: FilesTree -> Bool
isDir Dir{..} = True
isDir File{..} = False

getTreeName :: FilesTree -> FilePath
getTreeName Dir{..} = takeBaseName path
getTreeName File{..} = takeFileName path

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

printFT :: Int -> FilesTree -> IO()
printFT cnt File{..}  = putStrLn $ duplicate "-" cnt ++ ">" ++ path
printFT cnt Dir{..}  = do
  putStrLn $ duplicate "-" cnt ++ ">" ++ path
  forM_ children (printFT (cnt + 1))

duplicate :: String -> Int -> String
duplicate string n = Prelude.concat $ replicate n string

rootDir :: FilePath
rootDir = "/home/damm1t/Workspace/TestFolder"

readFS :: FilePath -> IO FilesTree
readFS curPath = do
  isDirExist <- doesDirectoryExist curPath
  if isDirExist then do
    list <- readDir curPath
    return $ Dir list curPath
  else
    return $ File curPath

readDir :: FilePath -> IO [FilesTree]
readDir curPath = do listDirs <- listDirectory curPath
                     traverse (readFS . (\ name -> curPath ++ "/" ++ name)) listDirs


testFunc :: IO ()
testFunc = do
  fs <- readFS rootDir
  printFT 0 fs
  
--opts :: ParserInfo Command
--opts = info (commands <**> helper) idm
{-



data ParamsCD = ParamsCD { dir :: String
                , flag :: Bool
                }
runWithOptions :: ParamsCD -> IO ()
runWithOptions = do
  isDirExist <- doesDirectoryExist curPath
  if isDirExist then do
    list <- readDir curPath
    return $ Dir list curPath
  else
    return $ File curPath

cd :: IO ()
cd = execParserWithHelp defaultPrefs opts >>= runWithOptions-- execParser opts >>= runWithOptions
  where
    parser = MyApp <$> argument str (metavar "NAME")
                   <*> switch (short 'e' <>
                               long "excited" <>
                               help "Run in excited mode")
    opts = info parser mempty-}
