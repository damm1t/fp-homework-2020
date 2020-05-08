module Main where

import ReadFileSystem
import Commands
import System.IO (hFlush, stdout)
import System.Directory (doesDirectoryExist)

main :: IO ()

main = do
  putStrLn "Enter root directory path"
  hFlush stdout
  root <- getLine
  isDirExist <- doesDirectoryExist root
  if not isDirExist then
    do
      fs <- readFS rootDir
      commandsParser (fs, rootDir)
  else
    do
      fs <- readFS root
      commandsParser (fs, root)
