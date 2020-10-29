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
main = formMain
-- main :: IO ()
-- main = do
--   --we actually only need last False from this junk init
--   let initialState = Game (Pos 1 1) (freshBoard 3) Unknown Cross Circle False
--   finalState <- defaultMain app initialState
--   return ()
