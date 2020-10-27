{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module Client
  (
    handleEvent, triplePositions
  ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Servant.Types.SourceT (foreach)
import Types
import BoardUtils
import Server(api)
import Data.IORef
import Lens.Micro 
import GameType
import qualified Servant.Client.Streaming as S
import Brick
import qualified Graphics.Vty as V

askGameId :: IO Int

askMove :: MoveRequest -> IO Board

simple :: IO Board

monadTransform :: ClientM a -> IO a
monadTransform clientma = do 
  manager <- newManager defaultManagerSettings
  let cenv = mkClientEnv manager (BaseUrl Http "localhost" 8081 "")
  value <- (runClientM clientma cenv)
  case value of 
    Left err -> error $ show err
    Right x  -> return x

askGameId :<|> askMove :<|> simple = hoistClient api monadTransform (client api) 

moveSelection :: Int -> (Int -> Int) -> (Int -> Int) 
moveSelection n f = \x -> helper n (f x) where
  helper a b | (b == (-1))  = a - 1
             | (b == a)     = 0
             | otherwise    = b

triplePositions :: Int -> [[Pos]]
triplePositions n = ditchEmpty [ makeSequence n x y vec | x <- [0..n - 1], y <- [0..n - 1], vec <- vecs ] where
  vecs = [(1, 0), (0, 1), (1, 1)]
  ditchEmpty :: [[Pos]] -> [[Pos]]
  ditchEmpty []     = []
  ditchEmpty (x:xs) = if (length x == 0) then (ditchEmpty xs) else (x : (ditchEmpty xs)) 
  makeSequence :: Int -> Int -> Int -> (Int, Int) -> [Pos]
  makeSequence n x y (dx, dy) | (x + 2 * dx >= n) || (y + 2 * dy >= n) = []
                              | otherwise                              = [ Pos x y, Pos (x + dx) (y + dy), Pos (x + 2 * dx) (y + 2 * dy) ]

isOver :: Game -> Bool
isOver g = ((setOverResult g)^.result /= Unknown)

setOverResult :: Game -> Game
setOverResult g = g{_result = collect [ isOver (g^.board.objects) pos | pos <- (triplePositions (g^.board.sz)) ]} where
  collect :: [GameResult] -> GameResult
  collect [] = Unknown
  collect (x:xs) = case x of
    Victory   -> Victory
    Loss      -> Loss
    otherwise -> collect xs
  isOver :: [Cell] -> [Pos] -> GameResult
  isOver objs ps = collect [isTriple objs ps Cross, isTriple objs ps Circle]
  isTriple :: [Cell] -> [Pos] -> CellType -> GameResult
  isTriple _ [] Cross            = Victory
  isTriple _ [] Circle           = Loss
  isTriple objects (p:ps) ct = case samePos of
    Just c  -> isTriple objects ps ct
    Nothing -> Unknown
    where 
      samePos = objects ^? traversed . filtered (\x -> (x^.coord == p) && (x^.ctype == ct))


setDrawResult :: Game -> Game
setDrawResult g = if (length (g^.board.objects) == (g^.board.sz) * (g^.board.sz)) then g{_result = Draw} else g

gameOnGuard :: Game -> EventM Name (Next Game) -> EventM Name (Next Game)
gameOnGuard g m = do 
  case (g^.result) of
    Unknown   -> m
    otherwise -> continue $ g

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g

handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))

handleEvent g (VtyEvent (V.EvKey V.KRight []))      = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x + 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))

handleEvent g (VtyEvent (V.EvKey V.KUp []))         = do
  continue $ g & (selection.x) %~ (moveSelection (g^.board.sz) (\x -> x - 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))

handleEvent g (VtyEvent (V.EvKey V.KDown []))       = do
  continue $ g & (selection.x) %~ (moveSelection (g^.board.sz) (\x -> x + 1))

handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = do
  continue $ g & (selection.y) %~ (moveSelection (g^.board.sz) (\x -> x - 1))

handleEvent g (VtyEvent (V.EvKey V.KEnter []))      = gameOnGuard g $ do 
  let aftermove = g{_board = putCell (Cell Cross (g^.selection)) (g^.board)}   
  if isOver aftermove 
  then
    continue aftermove
  else do
    afteraimove <- liftIO $ askMove $ MoveRequest (aftermove^.board) 12
    continue $ setOverResult $ setDrawResult (g & board .~ afteraimove)
  
handleEvent g _                                     = continue g

