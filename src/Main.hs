import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List(iterate)

import Control.Concurrent(forkIO,threadDelay)
import Control.Concurrent.STM
import Control.Monad.Trans(liftIO)
import Control.Monad(forM)

import System.Log.Logger
import Debug.Trace

import KTable
import Globals
import Comm
import Kad

newNode myPort otherPort = do
  trt <- newTVarIO M.empty
  trot <- newTVarIO M.empty
  tkt <- newTVarIO $ kbleaf S.empty
  -- nodeId = port
  debug $ "Starting on port " ++ show myPort
  forkIO $ runServer trt trot tkt (Peer "127.0.0.1" (show myPort) myPort) (start myPort otherPort)

start myPort otherPort = do
  tunnel serverDispatch (localServer $ show myPort)
  startNode $ Peer "127.0.0.1" (show otherPort) otherPort
  liftIO . threadDelay $ 20*1000*1000
  kt <- readKTree
  liftIO . putStrLn . show . ktreeList $ kt

debug = debugM "Main"

main = do
  updateGlobalLogger "Main" (setLevel DEBUG)
  updateGlobalLogger "Kad" (setLevel DEBUG)

  forM (take 5 $ iterate (+1) 2000) (\x -> if x == 2000 then newNode x 0 else newNode x (x-1) )
  liftIO . threadDelay $ 30*1000*1000
