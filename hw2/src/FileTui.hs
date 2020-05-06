{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FileTui where

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import           Brick.Widgets.Core
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import qualified Data.ByteString.Char8 as BS

tui :: BS.ByteString -> IO ()
tui st = do
  initialState <- buildInitialState st
  endState <- defaultMain tuiApp initialState
  print endState

newtype TuiState
  = TuiState {tuiStatePaths :: [SimpleFile]}
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
    , appAttrMap = const $ attrMap mempty [("exit", fg yellow), ("file", fg blue)]
    }

data SimpleFile = SData {dataText :: String} | ExitMsg {msg :: String } deriving (Show, Eq, Ord)

buildInitialState :: BS.ByteString -> IO TuiState
buildInitialState st = do
  let dataText = SData $ BS.unpack st
  let msg = ExitMsg "Press Q to quit"
  pure TuiState {tuiStatePaths = dataText : [msg]}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [vBox $ map drawPath $ tuiStatePaths ts]

drawPath :: SimpleFile -> Widget n
drawPath SData{..} = withAttr "file" $ str dataText
drawPath ExitMsg{..} = withAttr "exit" $ str msg

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s