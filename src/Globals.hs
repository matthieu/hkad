{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 

module Globals (
    RunningOps(..), ServerState, KadOp(..), GlobalData(..), HandlerFn(..),
    runServer, askRoutingT, askRunningOpsT, askKTree, readKTree, askLocalId,
    newRunningLookup, runningLookup, runningLookupDone, 
    newWaitingReply, waitingReply, newUid, insertInKTree,
    insertInStore, lookupInStore, deleteInStore,
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
             | StoreOp | StoreReplyOp | ValueLookupOp | ValueLookupReplyOp
  deriving (Show, Eq)

data WaitingReply = WaitingReply { waitFromPeer:: Peer,  waitOp:: KadOp, waitOpUid:: Word64 }
  deriving (Show, Eq)

type RunningOpsTable = M.Map Word64 RunningOps

data RunningOps = 
  RunningLookup { lookupNodeId ::Integer,  known:: [Peer], pending:: [Peer], 
                  queried:: [Peer], lookupHandler:: HandlerFn }

data HandlerFn = PeersHandler ([Peer] -> ServerState ())
               | ContentHandler (Maybe String -> ServerState ())

data GlobalData = GlobalData { 
  routingTable    :: TVar RoutingTable, 
  runningOpsTable :: TVar RunningOpsTable, 
  globalKTree     :: TVar KTree, 
  localPeer       :: Peer,
  localStore      :: TVar (M.Map Integer String)
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
askLocalStore = liftM localStore ask

readRoutingT = do { rt <- askRoutingT; liftIO . atomically $ readTVar rt }
readRunningOpsT = do { rot <- askRunningOpsT; liftIO . atomically $ readTVar rot }
readKTree = do { kt <- askKTree; liftIO . atomically $ readTVar kt }

modifyTVar :: (a -> a) -> TVar a -> STM ()
modifyTVar f tv = readTVar tv >>= writeTVar tv . f

-- Helper to execute atomically a ST action on a value asked in ServerState
atomicOnAsk :: (MonadIO m) => m a -> (a -> STM b) -> m b
atomicOnAsk asked action = asked >>= liftIO . atomically . action

insertInStore k v = atomicOnAsk askLocalStore (modifyTVar $ M.insert k v)

lookupInStore k = liftM (M.lookup k) $ atomicOnAsk askLocalStore readTVar 

deleteInStore k = atomicOnAsk askLocalStore (\lst -> do
  ls  <- readTVar lst
  case M.lookup k ls of
    Nothing -> return False
    Just v  -> modifyTVar (M.delete k) lst >> return True )

-- Inserts a new running lookup in the table of current operations
--
newRunningLookup nid rs ps qs lookupId handlerFn =
  let nrl = RunningLookup nid rs ps qs handlerFn
  in atomicOnAsk askRunningOpsT $ modifyTVar (M.insert lookupId nrl)

runningLookup lookupId = liftM (M.lookup lookupId) readRunningOpsT

runningLookupDone:: Word64 -> ServerState (Maybe RunningOps)
runningLookupDone lookupId = atomicOnAsk askRunningOpsT (\rot -> do
  ro <- readTVar rot
  let val = M.lookup lookupId ro
  case val of
    Nothing -> return Nothing
    Just wr -> modifyTVar (M.delete lookupId) rot >> return val )

newWaitingReply:: Peer -> KadOp -> Word64 -> Word64 -> ServerState ()
newWaitingReply peer op msgId opId = 
  atomicOnAsk askRoutingT $ modifyTVar (M.insert msgId $ WaitingReply peer op opId)

waitingReply:: Word64 -> KadOp -> Peer -> ServerState (Maybe Word64)
waitingReply msgId op peer = atomicOnAsk askRoutingT (\trt -> do
  rt  <- readTVar trt
  case M.lookup msgId rt of
    Nothing -> return Nothing
    Just wr -> do
      modifyTVar (M.delete msgId) trt
      return $ if waitFromPeer wr == peer && waitOp wr == op 
                then Just (waitOpUid wr) 
                else Nothing )

insertInKTree peer = do
  lid <- askLocalId
  atomicOnAsk askKTree $ modifyTVar (flip (kinsert $ nodeId lid) $ peer)

newUid:: IO Word64
newUid = liftM fromInteger $ randomRIO (0, 2^64 - 1)

