{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where
    
import Data.Aeson
import GHC.Generics
import Lens.Micro.TH (makeLenses)

data CellType = Cross | Circle | Dot deriving (Show, Generic, Eq)

data Pos = Pos 
  {
    _x :: Int, 
    _y :: Int
  } deriving (Show, Generic, Eq)

makeLenses ''Pos

data Cell = Cell 
  {
  _ctype :: CellType,
  _coord :: Pos
  } deriving (Show, Generic, Eq)

makeLenses ''Cell

instance ToJSON Pos
instance ToJSON CellType
instance ToJSON Cell
instance FromJSON Pos
instance FromJSON CellType
instance FromJSON Cell

data GameResult = Victory | Loss | Draw | Unknown deriving (Eq, Show)