module ClientMain where

import Brick
import Lens.Micro((^.))

import Client
import GameType
import Types
import UI

app :: App Game Tick Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

main :: IO ()
main = do
  initialState <- formMain
  if initialState^.playerCell == Dot
  then
    return ()
  else do
    _ <- defaultMain app initialState
    return ()