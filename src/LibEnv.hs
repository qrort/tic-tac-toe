{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module LibEnv where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.IORef
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Types
import BoardUtils
import Control.Monad.Reader

data Game = Game 
  {
  _gid       :: Int,
  _selection :: Pos, 
  _board     :: Board
  }

makeLenses ''Game

--type AppM a = ReaderT Game IO a

