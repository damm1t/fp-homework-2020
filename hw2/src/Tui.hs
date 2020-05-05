{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events
import Data.List (sort)

import FileDirectory

tui :: (FilesTree, FilePath) -> IO ()
tui (tree, path) = do
  initialState <- buildInitialState (tree, path)
  endState <- defaultMain tuiApp initialState
  print endState

newtype TuiState
  = TuiState {tuiStatePaths :: [FilePath]}
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }
  
buildInitialState :: (FilesTree, FilePath) -> IO TuiState
buildInitialState (tree, path) = do
  let here = map getTreeName (children $ getCurrentTree tree path)
  pure TuiState { tuiStatePaths = sort here }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [vBox $ map drawPath $ tuiStatePaths ts]

drawPath :: FilePath -> Widget n
drawPath = str

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s