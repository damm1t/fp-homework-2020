module Main where

import ReadFileSystem
import FileDirectory
import Commands

main :: IO ()

main = do
  fs <- readFS rootDir
  printFT 0 fs
  commandsParser (fs, rootDir)
