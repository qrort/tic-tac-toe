{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module Types
  (
    Cell(..), 
    CellType(..),
    Pos(..),
    coord, x, y, ctype,
    fromInt
  ) where
    
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Data.Time
import Data.Text
import Lens.Micro.TH (makeLenses)


data CellType = Cross | Circle | Dot deriving (Generic, Eq)

fromInt :: Integer -> CellType
fromInt 0 = Cross
fromInt 1 = Circle
fromInt _ = error "invalid argument to fromInt"

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