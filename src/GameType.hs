{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module GameType where

import Lens.Micro.TH
import Types
import BoardUtils

data Game = Game 
  {
  _selection  :: Pos, 
  _board      :: Board,
  _result     :: GameResult,
  _playerCell :: CellType,
  _aiCell     :: CellType,
  _row        :: Int
  } deriving (Show)

makeLenses ''Game

data Cfg = Cfg 
  {
  _len   :: Int,
  _win   :: Int,
  _pcell :: CellType
  } deriving (Show)

makeLenses ''Cfg 

data Tick = Tick

data Name = LenField
          | WinField
          | CrossField
          | CircleField
          deriving (Eq, Ord, Show)