module ClientMain where

import Client
import Types
import GameType
import Brick
import UI
import BoardUtils

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
  let initialState = Game 1 (Pos 1 1) (freshBoard 3) Unknown
  finalState <- defaultMain app initialState
  return ()
