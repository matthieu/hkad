import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List(iterate)

import Data.Digest.Pure.SHA
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Internal as BI

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

intSHA1 n = integerDigest . sha1 $ BI.chunk (B.pack . show $ n) BI.Empty

newNode myPort otherPort = do
  trt  <- newTVarIO M.empty
  trot <- newTVarIO M.empty
  tkt  <- newTVarIO $ kbleaf S.empty
  ls   <- newTVarIO M.empty
  let myId = intSHA1 myPort
  let gd   = GlobalData trt trot tkt (Peer "127.0.0.1" (show myPort) myId) ls
  -- nodeId = sha1 port
  debug $ "Starting on port " ++ show myPort
  forkIO $ runServer gd (start myPort otherPort)
  liftIO . threadDelay $ 100*1000

start myPort otherPort = do
  let peerId = intSHA1 otherPort
  tunnel serverDispatch (localServer $ show myPort)
  startNode $ Peer "127.0.0.1" (show otherPort) peerId
  liftIO . threadDelay $ 10*1000*1000
  if myPort `mod` 70 == 0 
    then do
      liftIO . putStrLn $ show myPort ++ " Store and retrieve"
      store (intSHA1 $ myPort * 11) "foobar"
      liftIO . threadDelay $ 2*1000*1000
      valueLookup (intSHA1 $ myPort * 11) $ ContentHandler (\s-> liftIO . putStrLn . ("### Found " ++) . show $ s)
    else return ()
  kt <- readKTree
  liftIO . putStrLn . (\x->show myPort ++ " " ++ x) . show . length . ktreeList $ kt
  liftIO . threadDelay $ 500*1000*1000

debug = debugM "Main"

main = do
  updateGlobalLogger "Main" (setLevel INFO)
  updateGlobalLogger "Kad" (setLevel INFO)

  forM (take 200 $ iterate (+1) 2000) (\x -> if x == 2000 then newNode x 0 else newNode x (x-1) )
  liftIO . threadDelay $ 22*1000*1000
