{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 

module Globals (
    RunningOps(..), ServerState, KadOp(..), GlobalData(..),
    runServer, askRoutingT, askRunningOpsT, askKTree, readKTree, askLocalId,
    newRunningLookup, runningLookup, runningLookupDone, 
    newWaitingReply, waitingReply, newUid, insertInKTree,
    modifyTVar
  ) where

import Data.Word
import Control.Monad.Reader
import Control.Concurrent.STM
import System.Random

import qualified Data.Map as M
import Debug.Trace

import KTable

type RoutingTable = M.Map Word64 WaitingReply

data KadOp = UnknownOp | NodeLookupOp | NodeLookupReplyOp | PingOp | PingReplyOp
  deriving (Show, Eq)

data WaitingReply = WaitingReply { waitFromPeer:: Peer,  waitOp:: KadOp, waitOpUid:: Word64 }
  deriving (Show, Eq)

type RunningOpsTable = M.Map Word64 RunningOps

data RunningOps = 
  RunningLookup { lookupNodeId ::Integer,  known:: [Peer], pending:: [Peer], 
                  queried:: [Peer], lookupHandler:: ([Peer]->ServerState ()) }

data GlobalData = GlobalData { 
  routingTable    :: TVar RoutingTable, 
  runningOpsTable :: TVar RunningOpsTable, 
  globalKTree     :: TVar KTree, 
  localPeer       :: Peer,
  localStore      :: M.Map Integer String 
}

newtype ServerState a = ServerState {
  runSS:: ReaderT GlobalData IO a 
} deriving (Monad, MonadIO, MonadReader GlobalData)

-- runServer rt rot kt localId st = runReaderT (runSS st) (rt, rot, kt, localId)
runServer gd st = runReaderT (runSS st) gd

askRoutingT = liftM routingTable ask
askRunningOpsT = liftM runningOpsTable ask
askKTree = liftM globalKTree ask
askLocalId = liftM localPeer ask

readRoutingT = do { rt <- askRoutingT; liftIO . atomically $ readTVar rt }
readRunningOpsT = do { rot <- askRunningOpsT; liftIO . atomically $ readTVar rot }
readKTree = do { kt <- askKTree; liftIO . atomically $ readTVar kt }

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = readTVar tv >>= writeTVar tv . f

-- Inserts a new running lookup in the table of current operations
--
newRunningLookup nid rs ps qs lookupId handlerFn = do
  rot <- askRunningOpsT
  let nrl = RunningLookup nid rs ps qs handlerFn
  liftIO . atomically $ modifyTVar rot (M.insert lookupId nrl)

runningLookup lookupId = do
  ro <- readRunningOpsT
  return $ M.lookup lookupId ro

runningLookupDone:: Word64 -> ServerState Bool
runningLookupDone lookupId = do
  rot <- askRunningOpsT
  liftIO . atomically $ do
    ro <- readTVar rot
    case M.lookup lookupId ro of
      Nothing -> return False
      Just wr -> do
        modifyTVar rot $ M.delete lookupId
        return True

newWaitingReply:: Peer -> KadOp -> Word64 -> Word64 -> ServerState ()
newWaitingReply peer op msgId opId = do
  rt <- askRoutingT
  liftIO . atomically $ modifyTVar rt (M.insert msgId $ WaitingReply peer op opId)

waitingReply:: Word64 -> KadOp -> Peer -> ServerState (Maybe Word64)
waitingReply msgId op peer = do
  trt <- askRoutingT
  liftIO . atomically $ do 
    rt  <- readTVar trt
    case M.lookup msgId rt of
      Nothing -> return Nothing
      Just wr -> do
        modifyTVar trt $ M.delete msgId
        return $ if waitFromPeer wr == peer && waitOp wr == op then Just (waitOpUid wr) else Nothing 

insertInKTree peer = do
  kt  <- askKTree
  lid <- askLocalId
  liftIO . atomically $ modifyTVar kt (flip (kinsert $ nodeId lid) $ peer)

newUid:: IO Word64
newUid = liftM fromInteger $ randomRIO (0, 2^64 - 1)

