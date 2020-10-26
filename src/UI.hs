{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Lens.Micro
import LibEnv
import Types
import BoardUtils

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

-- App definition


app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))      = continue $ g{_board = putCell (Cell Cross (g^.selection)) (g^.board)}    
handleEvent g _ = continue g
-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (12)
         , padTop (Pad 2) $ drawGameOver False
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "TicTacToe")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [0..n-1]]
    n            = g ^. board . sz 
    cellsInRow x = [drawCoord $ Pos x y | y <- [0..n-1]]
    drawCoord    = drawCell . cellAt
    cellAt p     = if length samePos > 0
      then
        (head samePos)^.ctype
      else
        Dot
          where 
            samePos = (g ^.. board . objects . traverse . filtered (\x -> (x^.coord == p)))
drawCell :: CellType -> Widget Name
drawCell Cross  = withAttr crossAttr xSymbol
drawCell Circle = withAttr circleAttr oSymbol
drawCell Dot    = withAttr dotAttr dotSymbol

xSymbol :: Widget Name
xSymbol = str "X"

oSymbol :: Widget Name
oSymbol = str "O"

dotSymbol :: Widget Name
dotSymbol = str "."


crossAttr, circleAttr, dotAttr :: AttrName
crossAttr = "X"
circleAttr = "O"
dotAttr = "."

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (crossAttr, V.blue `on` V.blue)
  , (circleAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]