{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE InstanceSigs    #-}

module Client
  (
    --runclient,
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
import Server(API)
import Data.IORef
import Lens.Micro ((&), (.~), (%~), (^.))
import LibEnv
import qualified Servant.Client.Streaming as S

-- api :: Proxy API
-- api = Proxy

-- askGameId :: AppM Int

-- askMove :: Board -> Int -> AppM Board

-- monadTransform :: ClientM a -> AppM a
-- monadTransform clientma = ReaderT $ \env -> do
--   manager <- newManager defaultManagerSettings
--   let cenv = mkClientEnv manager (BaseUrl Http "localhost" 8081 "")
--   value <- (runClientM clientma cenv)
--   case value of 
--     --There is no real reason for a ClientError except that server and client code is out of sync
--     --I did not come with a way to serve a value of type 'a' in this branch,
--     --And I do not have an idea how to provide user with a good-looking message like
--     --'Please git pull'
--     --However, this application is not going to be maintained, so I can leave it as is?
--     Left err -> error "client and server code versions are out of sync"
--     Right x  -> return x

-- askGameId :<|> askMove = hoistClient api monadTransform (client api) 

-- myWriteIORef x y = liftIO $ writeIORef x y

-- runclient :: AppM ()
-- runclient = do
--   --start game button
--   --ask game id
--   gameid <- askGameId
--   liftIO $ putStrLn (show gameid)
--   env <- ask
--   myWriteIORef (env^.gid) gameid
--   --request move via input
--   --send move to client
--   --accept client response
--   --write client response to ioref