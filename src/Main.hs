import qualified Data.Map as M
import qualified Data.Sequence as S
import Control.Concurrent(forkIO,threadDelay)
import Control.Concurrent.STM
import System.Environment(getArgs)
import Control.Monad.Trans(liftIO)
import System.Log.Logger
import Debug.Trace

import KTable
import Globals
import Comm
import Kad

start myPort otherPort nodeId = do
  tunnel serverDispatch (localServer myPort)
  if read otherPort > 0 
    then insertInKTree $ Peer "127.0.0.1" otherPort (read otherPort)
    else return ()
  if nodeId > 0 then nodeLookup nodeId else return ()
  liftIO $ threadDelay (60*1000*1000)

debug = debugM "Main"

main = do
  updateGlobalLogger "Main" (setLevel DEBUG)
  updateGlobalLogger "Kad" (setLevel DEBUG)

  params <- getArgs
  let myPort = head params
  debug $ "Starting on port " ++ show myPort

  trt <- newTVarIO M.empty
  trot <- newTVarIO M.empty
  tkt <- newTVarIO $ kbleaf S.empty
  -- nodeId = port
  runServer trt trot tkt (Peer "127.0.0.1" myPort (read myPort)) (start (params !! 0) (params !! 1) (read $ params !! 2))
