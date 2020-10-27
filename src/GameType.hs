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
  _gid       :: Int,
  _selection :: Pos, 
  _board     :: Board,
  _result    :: GameResult
  }

makeLenses ''Game

data Tick = Tick

type Name = ()