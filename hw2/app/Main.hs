module Main where

import FileDirectory
import Commands

main :: IO ()

main = do
  fs <- readFS rootDir
  commandsParser (fs, rootDir)
