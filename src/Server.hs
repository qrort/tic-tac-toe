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
import Data.Time
import Data.Text
import Types
import BoardUtils
import Lens.Micro ((&), (.~), (%~), (^.))

type API = "startgame" :> Get '[JSON] Int
  :<|> "requestmove" :> ReqBody '[JSON] MoveReq :> Get '[JSON] Board
  :<|> "simple" :> Get '[JSON] Board

server :: Server API
server = startgame
    :<|> move
    :<|> simple
  where 
    startgame :: Handler Int
    startgame = return 123

    move :: MoveReq -> Handler Board
    move req = do 
      return $ aimove Circle (req^.reqboard)

    simple :: Handler Board
    simple = do
      return $ freshBoard 3


api :: Proxy API
api = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

runserver :: IO ()
runserver = run 8081 app