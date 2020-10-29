{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module BoardUtils
  (
    Board(..),
    MoveRequest(..),
    sz, objects,
    aimove,
    freshBoard,
    putCell,
    reqboard,
    reqcell, 
    emptySquares
  ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Data.Time
import Data.Text(Text, unpack, pack, concat)
import Prelude hiding (concat)
import Types
import Lens.Micro.TH (makeLenses)
import Lens.Micro
import System.Random (Random(..), StdGen(..), mkStdGen, randomR)
import Data.List.Split
import Data.Aeson (encode)

data Board = Board
  {
    _sz          :: Int,
    _objects     :: [Cell]
  } deriving (Show, Generic)

makeLenses ''Board

instance ToJSON Board
instance FromJSON Board

data MoveRequest = MoveRequest
  {
    _reqboard :: Board,
    _reqcell  :: CellType
  } deriving Generic

makeLenses ''MoveRequest

instance ToJSON MoveRequest
instance FromJSON MoveRequest

freshBoard :: Int -> Board
freshBoard len = Board{_sz = len, _objects = []}

okCell :: Cell -> Board -> Bool
okCell c b = 
  (c^.coord.x >= 0 
  && c^.coord.y >= 0 
  && c^.coord.x < b^.sz 
  && c^.coord.y < b^.sz) 
  && (not (elem (c^.coord) (b ^.. objects . traversed . coord)))

putCell :: Cell -> Board -> Board
putCell c b | okCell c b = b{_objects = c : (b^.objects)} 
            | otherwise  = b

--rewrite
firstValidFromList :: [Pos] -> CellType -> Board -> Board
firstValidFromList [] _ b = b
firstValidFromList (x:xs) ct b | okCell nc b = putCell nc b
                               | otherwise   = firstValidFromList xs ct b
                                  where
                                    nc = Cell{_ctype = ct, _coord = x}

aimove :: CellType -> Board -> Board
aimove ct b = firstValidFromList [Pos x y | x <- [0..(b^.sz - 1)], y <- [0..(b^.sz - 1)]] ct b

emptySquares :: Board -> [Pos]
emptySquares b = ditchEmpty [aval b $ Pos x y | x <- [0..b^.sz - 1], y <- [0..b^.sz - 1]] where
  ditchEmpty :: [Maybe Pos] -> [Pos]
  ditchEmpty []     = []
  ditchEmpty (x:xs) = case x of
    Just p  -> p : ditchEmpty xs
    Nothing -> ditchEmpty xs
  aval :: Board -> Pos -> Maybe Pos
  aval b p = case (b ^? objects . traversed . coord .filtered (== p)) of
    Just x  -> Nothing
    Nothing -> Just p

