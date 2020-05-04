{-# LANGUAGE RecordWildCards #-}

module FileDirectory 
  ( FilesTree(..)
  , readFS
  , rootDir
  , isDir
  )where

import System.Directory(listDirectory, doesDirectoryExist)
import Control.Monad.Cont (forM_)
import Options.Applicative
import Options.Applicative.Help as AH
import Options.Applicative.Types as AT
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStr, stderr)

data FilesTree = File { path :: FilePath }
               | Dir { children  :: [FilesTree]
                     , path :: FilePath } deriving (Eq, Show)

isDir :: FilesTree -> Bool
isDir Dir{..} = True
isDir File{..} = False

printFT :: Int -> FilesTree -> IO()
printFT cnt File{..}  = putStrLn $ duplicate "-" cnt ++ ">" ++ path
printFT cnt Dir{..}  = do
  putStrLn $ duplicate "-" cnt ++ ">" ++ path
  forM_ children (printFT (cnt + 1))

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

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
