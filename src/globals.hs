{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 

module Globals (
    RunningOps(..), ServerState,KadOp(..),
    runServer, askRoutingT, askRunningOpsT, askKTree, readKTree,
    newRunningLookup, runningLookup, newWaitingReply, waitingReply, newUid,
    modifyTVar
  ) where

import Data.Word
import Control.Monad.Reader
import Control.Concurrent.STM
import System.Random

import qualified Data.Map as M

import KTable

type RoutingTable = M.Map Word64 WaitingReply

data KadOp = UnknownOp | NodeLookupOp | PingOp
  deriving (Show, Eq)

data WaitingReply = WaitingReply { waitFromPeer:: Peer,  waitOp:: KadOp, waitOpUid:: Word64 }

type RunningOpsTable = M.Map Word64 RunningOps

data RunningOps = RunningLookup { remaining:: [Peer], pending:: [Peer], queried:: [Peer], closest:: [Peer] }

type GlobalData = (TVar RoutingTable, TVar RunningOpsTable, TVar KTree)

newtype ServerState a = ServerState {
  runSS:: ReaderT GlobalData IO a 
} deriving (Monad, MonadIO, MonadReader GlobalData)

runServer rt rot kt st = runReaderT (runSS st) (rt, rot, kt)

askRoutingT = do { (rt, rot, kt) <- ask; return rt }
askRunningOpsT = do { (rt, rot, kt) <- ask; return rot }
askKTree = do { (rt, rot, kt) <- ask; return kt }

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

waitingReply msgId op peer = do
  rt <- readRoutingT
  case M.lookup msgId rt of
    Nothing -> return Nothing
    Just wr -> return $ if waitFromPeer wr == peer && waitOp wr == op then Just (waitOpUid wr) else Nothing

newUid:: IO Word64
newUid = liftM fromInteger $ randomRIO (0, 2^64 - 1)

