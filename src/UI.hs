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
  , (<=>)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Lens.Micro
import GameType
import Types
import BoardUtils
import Client
import Data.List.Split
import Data.List(intercalate)

drawUI :: Game -> [Widget Name]
drawUI g = [ (C.hCenter $ (drawGrid g)) <=> (C.hCenter $ padTop (Pad 2) (drawStats g)) ]

drawStats :: Game -> Widget Name
drawStats g = padTop (Pad 2) $ vBox 
  [ drawGameOver (g^.result)
  , drawBoard (g^.board)
  ]

drawBoard :: Board -> Widget Name
drawBoard b = C.hCenter $ str (intercalate "\nCell" (splitOn "Cell" $ show b))

drawGameOver :: GameResult -> Widget Name
drawGameOver x | x == Victory = withAttr gameOverAttr $ C.hCenter $ str "You won!"
               | x == Loss    = withAttr gameOverAttr $ C.hCenter $ str "You lost..."
               | x == Draw    = withAttr gameOverAttr $ C.hCenter $ str "Game drawn."
               | x == Unknown = emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

selectedAttr :: AttrName 
selectedAttr = "selected"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "TicTacToe")
  $ vBox rows
  where
    rows         = [ hBox $ cellsInRow r | r <- [0..n-1] ]
    n            = g ^. board . sz 
    cellsInRow x = [ drawCoord $ (Pos x y) | y <- [0..n-1] ]
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