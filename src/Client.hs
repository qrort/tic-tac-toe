{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module Client
  (
    askMove,
  ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Servant.Types.SourceT (foreach)
import Types
import BoardUtils
import Server(api)
import Data.IORef
import Lens.Micro ((&), (.~), (%~), (^.))
import LibEnv
import qualified Servant.Client.Streaming as S
import Brick (EventM(..))
import Control.Monad.State

askGameId :: IO Int

askMove :: MoveReq -> IO Board

simple :: IO Board

-- type AppM a = EventM Name (Next a)
-- runEventM :: ReaderT (EventRO Name) (StateT (EventState Name) IO) a
monadTransform :: ClientM a -> IO a
monadTransform clientma = do 
  manager <- newManager defaultManagerSettings
  let cenv = mkClientEnv manager (BaseUrl Http "localhost" 8081 "")
  value <- (runClientM clientma cenv)
  case value of 
    --There is no real reason for a ClientError except that server and client code is out of sync
    --I did not come with a way to serve a value of type 'a' in this branch,
    --And I do not have an idea how to provide user with a good-looking message like
    --'Please git pull'
    --However, this application is not going to be maintained, so I can leave it as is?
    Left err -> do
      error $ show err
    Right x  -> return x

askGameId :<|> askMove :<|> simple = hoistClient api monadTransform (client api) 

--curl -X POST -d '{\"_sz\":3,\"_objects\":[]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/simple
