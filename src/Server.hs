{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module Server
  (
    API, 
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

type API = "startgame" :> Get '[JSON] Int
  :<|> "requestmove" :> ReqBody '[JSON] Board :> ReqBody '[JSON] Int :> Get '[JSON] Board

server :: Server API
server = startgame
    :<|> move
  where startgame :: Handler Int
        startgame = return 123

        move :: Board -> Int -> Handler Board
        move board gameid = do 
          return $ aimove Circle board


userAPI :: Proxy API
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI server

runserver :: IO ()
runserver = run 8081 app