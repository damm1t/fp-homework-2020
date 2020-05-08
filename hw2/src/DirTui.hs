{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DirTui where

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import           Brick.Widgets.Core
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import Data.List (sort)

import FileDirectory

tui :: (FilesTree, FilePath) -> IO ()
tui (tree, path) = do
  initialState <- buildInitialState (tree, path)
  endState <- defaultMain tuiApp initialState
  print endState

newtype TuiState
  = TuiState {tuiStatePaths :: [SimpleTree]}
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
    , appAttrMap = const $ attrMap mempty [ ("exit", fg yellow)
                                          , ("file", fg green)
                                          , ("directory", fg blue)
                                          ]
    }

data SimpleTree = SDir { name :: String }
                | SFile { name :: String }
                | ExitMsg { msg :: String }
                deriving (Show, Eq, Ord)

toTree :: FilesTree -> SimpleTree
toTree d@Dir{..} = SDir $ getTreeName d
toTree f@File{..} = SFile $ getTreeName f
toTree _ = undefined

buildInitialState :: (FilesTree, FilePath) -> IO TuiState
buildInitialState (tree, path) = do
  let here = map toTree (filter isTree (children $ getCurrentTree tree path))
  let msg = ExitMsg "Press Q to quit"
  pure TuiState {tuiStatePaths = sort here ++ [msg]}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [vBox $ map drawPath $ tuiStatePaths ts]

drawPath :: SimpleTree -> Widget n
drawPath SDir{..} = withAttr "directory" $ str name
drawPath SFile{..} = withAttr "file" $ str name
drawPath ExitMsg{..} = withAttr "exit" $ str msg

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s