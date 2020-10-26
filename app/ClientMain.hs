module ClientMain where

import Client
import Control.Monad.Reader
import Types
import LibEnv
import Data.IORef
import Brick
import Graphics.Vty as V
import UI
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)
import BoardUtils

-- main :: IO ()
-- main = do
--   defaultGameId <- newIORef $ GameId{_id=1}
--   runReaderT runclient Game{game_id = defaultGameId}

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000 -- decides how fast your game moves
  let g = Game 1 (Pos 1 1) (freshBoard 3) 
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app g

brickmain :: IO ()
brickmain = do
  -- let app = undefined
  -- initialState <- undefined
  -- finalState <- defaultMain app initialState
  return ()