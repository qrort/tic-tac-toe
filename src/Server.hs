{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module Server
  (
    api, 
    runserver
  ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Types
import BoardUtils
import Lens.Micro ((&), (.~), (%~), (^.))
import System.Random
import Control.Monad.IO.Class (liftIO) 

type API = "move" :> ReqBody '[JSON] MoveRequest :> Get '[JSON] Board

server :: Server API
server = move
  where 
    move :: MoveRequest -> Handler Board
    move req = do
      let list = emptySquares (req^.reqboard)
      id <- liftIO $ randomRIO (0, length list - 1)
      return $ putCell Cell{_ctype=(req^.reqcell), _coord=list!!id} (req^.reqboard)

api :: Proxy API
api = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

runserver :: IO ()
runserver = run 8081 app