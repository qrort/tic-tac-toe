{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module Client
  ( handleEvent
  , getFirstMove
  , setOverResult
  , setDrawResult
  ) where

import Brick
import Control.Monad.Reader
import Lens.Micro 
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client

import BoardUtils
import GameType
import Server(api)
import Types

import qualified Graphics.Vty as V

askMove :: MoveRequest -> IO Board

monadTransform :: ClientM a -> IO a
monadTransform clientma = do 
  manager <- newManager defaultManagerSettings
  let cenv = mkClientEnv manager (BaseUrl Http "localhost" 8081 "")
  value <- (runClientM clientma cenv)
  case value of 
    Left err -> error $ show err
    Right x  -> return x

askMove = hoistClient api monadTransform (client api) 

moveSelection :: Int -> (Int -> Int) -> (Int -> Int) 
moveSelection n f = \x -> helper n (f x) where
  helper a b | (b == (-1))  = a - 1
             | (b == a)     = 0
             | otherwise    = b

triplePositions :: Int -> Int -> [[Pos]]
triplePositions n r = ditchEmpty [ makeSequence n x y vec | x <- [0..n - 1], y <- [0..n - 1], vec <- vecs ] where
  vecs = [(1, 0), (0, 1), (1, 1), (1, -1)]
  ditchEmpty :: [[Pos]] -> [[Pos]]
  ditchEmpty []     = []
  ditchEmpty (x:xs) = if (length x == 0) then (ditchEmpty xs) else (x : (ditchEmpty xs)) 
  makeSequence :: Int -> Int -> Int -> (Int, Int) -> [Pos]
  makeSequence n x y (dx, dy) | (x + (r - 1) * dx >= n) || (y + (r - 1) * dy >= n) = []
                              | otherwise                              = [ Pos (x + k * dx)  (y + k * dy) | k <- [0..r - 1]]

isOverBool :: Game -> Bool
isOverBool g = ((setOverResult $ setDrawResult g)^.result /= Unknown)

setOverResult :: Game -> Game
setOverResult g | g^.result /= Draw = g{_result = collect [ isOver (g^.board.objects) pos | pos <- (triplePositions (g^.board.sz) (g^.row)) ]}
                | otherwise         = g where
  collect :: [GameResult] -> GameResult
  collect [] = Unknown
  collect (x:xs) = case x of
    Victory   -> Victory
    Loss      -> Loss
    _         -> collect xs
  isOver :: [Cell] -> [Pos] -> GameResult
  isOver objs ps = collect [isTriple objs ps Cross, isTriple objs ps Circle]
  isTriple :: [Cell] -> [Pos] -> CellType -> GameResult
  isTriple       _     [] ct | ct == (g^.playerCell) = Victory
                             | ct == (g^.aiCell)     = Loss
                             | otherwise             = error "wrong isTriple CellType"
  isTriple objs (p:ps) ct = case samePos of
    Just _  -> isTriple objs ps ct
    Nothing -> Unknown
    where 
      samePos = objs ^? traversed . filtered (\arg -> (arg^.coord == p) && (arg^.ctype == ct))

setDrawResult :: Game -> Game
setDrawResult g = if (length (g^.board.objects) == (g^.board.sz) * (g^.board.sz)) then g{_result = Draw} else g

gameOnGuard :: Game -> EventM Name (Next Game) -> EventM Name (Next Game)
gameOnGuard g m = do 
  case (g^.result) of
    Unknown   -> m
    _         -> continue $ g


getFirstMove :: Board -> IO Board
getFirstMove b = do
  afteraimove <- liftIO $ askMove $ MoveRequest b Cross
  return afteraimove

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g

handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\arg -> arg - 1))

handleEvent g (VtyEvent (V.EvKey V.KRight []))      = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\arg -> arg + 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\arg -> arg - 1))

handleEvent g (VtyEvent (V.EvKey V.KUp []))         = do
  continue $ g & (selection.x) %~ (moveSelection (g^.board.sz) (\arg -> arg - 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\arg -> arg - 1))

handleEvent g (VtyEvent (V.EvKey V.KDown []))       = do
  continue $ g & (selection.x) %~ (moveSelection (g^.board.sz) (\arg -> arg + 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\arg -> arg - 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  newBoard <- liftIO $ getFirstMove $ freshBoard (g^.board.sz)
  case (g^.playerCell) of 
    Cross  -> continue $ g & board .~ (freshBoard (g^.board.sz)) & result .~ Unknown
    Circle -> continue $ g & board .~ newBoard & result .~ Unknown
    _      -> error "no such playerCell"

handleEvent g (VtyEvent (V.EvKey V.KEnter []))      = gameOnGuard g $ do 
  let ok = okCell newCell (g^.board)
  if ok
  then do
    let aftermove = g{_board = putCell newCell (g^.board)}   
    if isOverBool aftermove
    then
      continue $ setOverResult $ setDrawResult aftermove
    else do
      afteraimove <- liftIO $ askMove $ MoveRequest (aftermove^.board) (aftermove^.aiCell)
      continue $ setOverResult $ setDrawResult (g & board .~ afteraimove)
  else
    continue g
  where
    newCell = Cell (g^.playerCell) (g^.selection)

handleEvent g _                                     = continue g
