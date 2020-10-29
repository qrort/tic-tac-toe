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
  , padRight, padLeft, padTop, padAll, padBottom, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  , (<=>)
  , fill
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Brick.Focus as F
import Lens.Micro
import GameType
import Types
import BoardUtils
import Client
import Data.List.Split
import Data.List(intercalate)
import Brick
import Brick.Forms

formApp :: App (Form Cfg e Name) e Name
formApp =
    App { appDraw = drawForm
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter []) -> do
                  let cur = F.focusGetCurrent $ formFocus s
                  case cur of
                    Just c  -> case c of
                      LenField    -> continue s
                      CrossField  -> continue $ setFormFocus CrossField (mkForm ((formState s) & pcell .~ Cross))
                      CircleField -> continue $ setFormFocus CircleField (mkForm ((formState s) & pcell .~ Circle))
                    Nothing -> continue s 
                _ -> do
                    s' <- handleFormEvent ev s
                    -- Example of external validation:
                    continue $ setFieldValid ((formState s')^.len <= 10 && (formState s')^.len > 0) LenField s'

        , appChooseCursor = neverShowCursor
        , appStartEvent = return
        , appAttrMap = const theMap
        }


formMain :: IO ()
formMain = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialCfg = Cfg { _len = 3
                         , _pcell = Cross
                         }

        f = mkForm initialCfg

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing formApp f

    putStrLn "The starting form state was:"
    print initialCfg

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')

drawUI :: Game -> [Widget Name]
drawUI g = [ (padTop (Pad 5) $ (C.hCenter $ (drawGrid g))) <=> (C.hCenter $ (drawStats g)) ]

mkForm :: Cfg -> Form Cfg e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                  (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Board size" @@= editShowableField len LenField
                , label "Your cell type" @@= radioField pcell
                  [ (Cross, CrossField, "Cross")
                  , (Circle, CircleField, "Circle")
                  ]
            ]

drawForm :: Form Cfg e Name -> [Widget Name]
drawForm f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Size must be an integer and not exceed 10\n" <>
                     "  (try entering an invalid size!)\n" <>
                     "- Cell type selects from a list of options\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

drawStats :: Game -> Widget Name
drawStats g = padTop (Pad 2) $ drawGameOver (g^.result)
-- drawStats g = padTop (Pad 2) $ vBox 
--   [ drawGameOver (g^.result)
--   --, drawBoard (g^.board)
--   ]

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


theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]