{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE FlexibleInstances #-}

module Types
  (
    Cell(..), 
    CellType(..),
    Pos(..),
    coord, x, y, ctype,
  ) where
    
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Data.Time
import Data.Text(Text, unpack, pack, concat)
import Lens.Micro.TH (makeLenses)
import Data.List.Split
import Data.Foldable(foldr')
import Lens.Micro ((&), (.~), (%~), (^.))
import Prelude hiding (concat)
import Data.Aeson (encode)

data CellType = Cross | Circle | Dot deriving (Generic, Eq)

data Pos = Pos 
  {
    _x :: Int, 
    _y :: Int
  } deriving (Generic, Eq)

makeLenses ''Pos

data Cell = Cell 
  {
  _ctype :: CellType,
  _coord :: Pos
  } deriving (Generic, Eq)

makeLenses ''Cell

instance ToJSON Pos
instance ToJSON CellType
instance ToJSON Cell
instance FromJSON Pos
instance FromJSON CellType
instance FromJSON Cell