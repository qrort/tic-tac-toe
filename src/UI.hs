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
import Client

app :: App Game Tick Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

moveSelection :: Int -> (Int -> Int) -> (Int -> Int) 
moveSelection n f = \x -> helper n (f x) where
  helper a b | (b == (-1))  = a - 1
             | (b == a)     = 0
             | otherwise    = b
    
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x + 1))
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ g & (selection.x) %~ (moveSelection (g^.board.sz) (\x -> x - 1))
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ g & (selection.x) %~ (moveSelection (g^.board.sz) (\x -> x + 1))
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))
handleEvent g (VtyEvent (V.EvKey V.KEnter []))      = do 
  let aftermove = g{_board = putCell (Cell Cross (g^.selection)) (g^.board)}    
  afteraimove <- liftIO $ askMove $ MoveReq (aftermove^.board) 12
  continue $ g & board .~ afteraimove
handleEvent g _                                     = continue g

-- Drawing

-- drawUI :: Game -> [Widget Name]
-- drawUI g =
--   [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ padRight (Pad 2) (drawSelection g) <+> drawGrid g]


drawSelection :: Game -> Widget Name
drawSelection g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ (show (g^.selection.x)) ++ " " ++ (show (g^.selection.y))

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

selectedAttr :: AttrName 
selectedAttr = "selected"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "TicTacToe")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [0..n-1]]
    n            = g ^. board . sz 
    cellsInRow x = [drawCoord $ (Pos x y) | y <- [0..n-1]]
    drawCoord p  = drawCell (cellAt p) p (g ^. selection)
    cellAt p     = case cellAtPos of 
      Just c  -> c^.ctype
      Nothing -> Dot
      where 
        cellAtPos = (g ^? board . objects . traverse . filtered (\x -> (x^.coord == p)))
          
appendAttrIf :: Bool -> AttrName -> Widget Name -> Widget Name
appendAttrIf True attr widget  = withAttr attr widget
appendAttrIf False attr widget = widget


drawCell :: CellType -> Pos -> Pos -> Widget Name
drawCell celltype pos target = appendAttrIf (pos == target) selectedAttr (cellWidget celltype) where
  cellWidget :: CellType -> Widget Name
  cellWidget Cross  = xSymbol
  cellWidget Circle = oSymbol
  cellWidget Dot    = dotSymbol

xSymbol :: Widget Name
xSymbol = str "X"

oSymbol :: Widget Name
oSymbol = str "O"

dotSymbol :: Widget Name
dotSymbol = str "."


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (selectedAttr, V.white `on` V.blue)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]