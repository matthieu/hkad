{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 

module Globals (
    RunningOps(..), ServerState, KadOp(..),
    runServer, askRoutingT, askRunningOpsT, askKTree, readKTree,
    newRunningLookup, runningLookup, newWaitingReply, waitingReply, newUid, insertInKTree,
    modifyTVar
  ) where

import Data.Word
import Control.Monad.Reader
import Control.Concurrent.STM
import System.Random

import qualified Data.Map as M

import KTable

type RoutingTable = M.Map Word64 WaitingReply

data KadOp = UnknownOp | NodeLookupOp | NodeLookupReplyOp | PingOp | PingReplyOp
  deriving (Show, Eq)

data WaitingReply = WaitingReply { waitFromPeer:: Peer,  waitOp:: KadOp, waitOpUid:: Word64 }

type RunningOpsTable = M.Map Word64 RunningOps

data RunningOps = RunningLookup { remaining:: [Peer], pending:: [Peer], queried:: [Peer], closest:: [Peer] }

type GlobalData = (TVar RoutingTable, TVar RunningOpsTable, TVar KTree, Integer)

newtype ServerState a = ServerState {
  runSS:: ReaderT GlobalData IO a 
} deriving (Monad, MonadIO, MonadReader GlobalData)

runServer rt rot kt localId st = runReaderT (runSS st) (rt, rot, kt, localId)

askRoutingT = do { (rt, rot, kt, lid) <- ask; return rt }
askRunningOpsT = do { (rt, rot, kt, lid) <- ask; return rot }
askKTree = do { (rt, rot, kt, lid) <- ask; return kt }
askLocalId = do { (rt, rot, kt, lid) <- ask; return lid }

readRoutingT = do { rt <- askRoutingT; liftIO . atomically $ readTVar rt }
readRunningOpsT = do { rot <- askRunningOpsT; liftIO . atomically $ readTVar rot }
readKTree = do { kt <- askKTree; liftIO . atomically $ readTVar kt }

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = readTVar tv >>= writeTVar tv . f

-- Inserts a new running lookup in the table of current operations
--
newRunningLookup rs ps lookupId = do
  rot <- askRunningOpsT
  let nrl = RunningLookup rs ps [] []
  liftIO . atomically $ modifyTVar rot (M.insert lookupId nrl)

runningLookup lookupId = do
  rot <- readRunningOpsT
  return $ M.lookup lookupId rot

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
  liftIO . atomically $ modifyTVar kt (flip (kinsert lid) $ peer)

newUid:: IO Word64
newUid = liftM fromInteger $ randomRIO (0, 2^64 - 1)

