{-# LANGUAGE OverloadedStrings #-}
module UI where

import Brick
  ( App(..), AttrMap, BrickEvent(..), Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padTop, padBottom, Padding(..)
  , withBorderStyle
  , str
  , withAttr, attrMap, emptyWidget, AttrName, on, fg
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
import Brick.Forms

formApp :: App (Form Cfg e Name) e Name
formApp =
    App { appDraw = drawForm
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey (V.KChar 'q') []) -> halt $ mkForm ((formState s) & pcell .~ Dot)
                VtyEvent (V.EvKey V.KEnter [])   -> if (allFieldsValid s) 
                then
                  halt s
                else
                  continue s

                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey (V.KChar ' ') []) -> do
                  let cur = F.focusGetCurrent $ formFocus s
                  case cur of
                    Just c  -> case c of
                      LenField    -> continue s 
                      WinField    -> continue s
                      CrossField  -> continue $ setFormFocus CrossField (mkForm ((formState s) & pcell .~ Cross))
                      CircleField -> continue $ setFormFocus CircleField (mkForm ((formState s) & pcell .~ Circle))
                    Nothing -> continue s 
                _ -> do
                    s' <- handleFormEvent ev s
                    continue $ 
                      setFieldValid ((formState s')^.win > 2 && (formState s')^.win <= (formState s')^.len) WinField
                      (setFieldValid ((formState s')^.len <= 10 && (formState s')^.len > 0) LenField s')

        , appChooseCursor = neverShowCursor
        , appStartEvent = return
        , appAttrMap = const theMap
        }


formMain :: IO Game
formMain = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse False
          return v

        initialCfg = Cfg { _len   = 3
                         , _win   = 3
                         , _pcell = Cross
                         }

        f = mkForm initialCfg

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing formApp f
    
    if ((formState f')^.pcell /= Dot)
    then do
      state <- cfgToGame (formState f')
      return state
    else do
      -- it is okay not to initialize other fields as
      -- Dot is used as exit flag 
      return Game{_playerCell = Dot}
    where
      cfgToGame :: Cfg -> IO Game
      cfgToGame cfg = do
        initBoard <- getFirstMove $ freshBoard (cfg^.len) 
        return $ Game { _selection  = Pos (cfg^.len `div` 2) (cfg^.len `div` 2)
                                    , _board      = if (cfg^.pcell) == Cross then freshBoard (cfg^.len) else initBoard
                                    , _result     = Unknown
                                    , _playerCell = cfg^.pcell
                                    , _aiCell     = if (cfg^.pcell) == Cross then Circle else Cross
                                    , _row        = cfg^.win
                                    } 

drawUI :: Game -> [Widget Name]
drawUI g = [ (padTop (Pad 5) $ (C.hCenter $ (drawGrid g))) <=> (C.hCenter $ (drawStats g)) ]

mkForm :: Cfg -> Form Cfg e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                  (vLimit 1 $ hLimit 20 $ str s <+> fill ' ') <+> w
    in newForm [  label "Board size" @@= editShowableField len LenField
                , label "In line to win" @@= editShowableField win WinField
                , label "Your cell type" @@= radioField pcell
                  [ (Cross, CrossField, "Cross")
                  , (Circle, CircleField, "Circle")
                  ]
            ]

drawForm :: Form Cfg e Name -> [Widget Name]
drawForm f = [C.center form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 70 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Size must be an integer and not exceed 10\n" <>
                     "  (try entering an invalid size!)\n" <>
                     "- Cell type selects from a list of options\n" <>
                     "- Number of characters in line to win should exceed 2\n" <>
                     "  and be less or equal to board size size\n" <>
                     "- Tab, Shift+Tab to switch, Space to select\n" <>
                     "- Enter to submit form. Q to quit\n" 

drawStats :: Game -> Widget Name
drawStats g = padTop (Pad 2) $ vBox 
  [ drawGameOver (g^.result)
  , drawLegend g
  ]

drawLegend :: Game -> Widget Name
drawLegend g = C.hCenter $ padTop (Pad 1) $ B.borderWithLabel (str "Rules") legend where
  legend | (g^.result == Unknown) = (str $ "arrows/WASD to move selection,\n" <>
                                    "Enter to place your symbol\n" <>
                                    "Q to quit, R to restart\n")
         | otherwise              = (str $ "R to play again\n")

drawBoard :: Board -> Widget Name
drawBoard b = C.hCenter $ str (intercalate "\nCell" (splitOn "Cell" $ show b))

drawGameOver :: GameResult -> Widget Name
drawGameOver g | g == Victory = withAttr victoryAttr $ C.hCenter $ str "You won!"
               | g == Loss    = withAttr lossAttr $ C.hCenter $ str "You lost..."
               | g == Draw    = withAttr drawAttr $ C.hCenter $ str "Game drawn."
               | otherwise    = emptyWidget

victoryAttr :: AttrName
victoryAttr = "victory"

lossAttr :: AttrName
lossAttr = "loss"

drawAttr :: AttrName
drawAttr = "draw"

selectedAttr :: AttrName 
selectedAttr = "selected"

comboAttr :: AttrName
comboAttr = "combo"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "<!>")
  $ vBox rows
  where
    winningRow   = findWinningRow g
    rows         = [ hBox $ cellsInRow r | r <- [0..n-1] ]
    n            = g ^. board . sz 
    cellsInRow a = [ drawCoord $ (Pos a b) | b <- [0..n-1] ]
    drawCoord p  = drawCell (cellAt p) p (g ^. selection) winningRow
    cellAt p     = case cellAtPos of 
      Just c  -> c^.ctype
      Nothing -> Dot
      where 
        cellAtPos = (g ^? board . objects . traverse . filtered (\arg -> (arg^.coord == p)))
          
appendAttrIf :: Bool -> AttrName -> Widget Name -> Widget Name
appendAttrIf True attr widget  = withAttr attr widget
appendAttrIf False _ widget    = widget


drawCell :: CellType -> Pos -> Pos -> [Pos] -> Widget Name
drawCell celltype pos target wr = 
  appendAttrIf (pos `elem` wr) comboAttr $
  appendAttrIf (pos == target) selectedAttr $ 
  (cellWidget celltype) 
  where
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
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (selectedAttr, V.black `on` V.yellow)
  , (lossAttr, fg V.red `V.withStyle` V.bold)
  , (victoryAttr, fg V.green `V.withStyle` V.bold)
  , (drawAttr, fg V.yellow `V.withStyle` V.bold)
  , (comboAttr, V.white `on` V.blue)
  ]