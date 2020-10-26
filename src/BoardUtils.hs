{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module BoardUtils
  (
    Board(..),
    sz, objects,
    aimove,
    freshBoard,
    putCell
  ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Data.Time
import Data.Text
import Types
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import System.Random (Random(..), StdGen(..), mkStdGen, randomR)

data Board = Board
  {
    _sz          :: Int,
    _objects     :: [Cell]
  } deriving Generic

makeLenses ''Board

instance ToJSON Board
instance FromJSON Board

freshBoard :: Int -> Board
freshBoard len = Board{_sz = len, _objects = []}

okCell :: Cell -> Board -> Bool
okCell c b = (c^.coord.x >= 0 && c^.coord.y >= 0 && c^.coord.x < b^.sz && c^.coord.y < b^.sz) && (not (elem c (b^.objects)))

putCell :: Cell -> Board -> Board
putCell c b | okCell c b = b{_objects = c : (b^.objects)} 
            | otherwise  = freshBoard $ b^.sz

--rewrite
firstValidFromList :: [Pos] -> CellType -> Board -> Board
firstValidFromList [] _ b = b
firstValidFromList (x:xs) ct b | okCell nc b = putCell nc b
                               | otherwise   = firstValidFromList xs ct b
                                  where
                                    nc = Cell{_ctype = ct, _coord = x}

aimove :: CellType -> Board -> Board
aimove ct b = firstValidFromList [Pos x y | x <- [0..(b^.sz)], y <- [0..(b^.sz)]] ct b