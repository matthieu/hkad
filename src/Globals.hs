{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 

module Globals (
    Peer(..),
    RunningOps(..), ServerState, KadOp(..), GlobalData(..), HandlerFn(..), WaitingReply(..),
    runServer, askRoutingT, askRunningOpsT, askKTree, readKTree, askLocalId,
    readRoutingT, readRunningOpsT,
    newRunningLookup, runningLookup, runningLookupDone, readStore, 
    newWaitingReply, waitingReply, nrandoms, newUid, insertInKTree,
    insertInStore, lookupInStore, deleteInStore,
    modifyTVar
  ) where

import Data.Word
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Applicative((<$>))

import qualified Data.Map as M
import Debug.Trace

import KTable

class Peer p where
  sendLookup      :: [p] -> Integer -> Word64 -> Bool -> ServerState p ()
  sendLookupReply :: p -> [p] -> Word64 -> Bool -> ServerState p () 
  sendStore       :: [p] -> Integer -> String -> Word64 -> ServerState p ()
  sendValueReply  :: p -> String -> Word64 -> ServerState p ()
  sendToPeer      :: String -> p -> IO ()
  serPeer         :: p -> String

type RoutingTable p = M.Map Word64 (WaitingReply p)

data KadOp = UnknownOp | NodeLookupOp | NodeLookupReplyOp | PingOp | PingReplyOp
             | StoreOp | StoreReplyOp | ValueLookupOp | ValueLookupReplyOp
  deriving (Show, Eq)

data WaitingReply p = WaitingReply { waitFromPeer:: p,  waitOp:: KadOp, waitOpUid:: Word64 }
  deriving (Show, Eq)

type RunningOpsTable p = M.Map Word64 (RunningOps p)

data RunningOps p = 
  RunningLookup { lookupNodeId ::Integer,  known:: [p], pending:: [p],
                  queried:: [p], lookupHandler:: HandlerFn p }

data HandlerFn p = PeersHandler ([p] -> ServerState p ())
                 | ContentHandler (Maybe String -> ServerState p ())

data GlobalData p = GlobalData { 
  routingTable    :: TVar (RoutingTable p),
  runningOpsTable :: TVar (RunningOpsTable p),
  globalKTree     :: TVar (KTree p),
  localPeer       :: p,
  localStore      :: TVar (M.Map Integer String),
  randoms         :: TVar [Integer]
}

newtype ServerState p a = ServerState {
  runSS:: ReaderT (GlobalData p) IO a 
} deriving (Monad, MonadIO, MonadReader (GlobalData p), Functor)

-- runServer rt rot kt localId st = runReaderT (runSS st) (rt, rot, kt, localId)
runServer gd st = runReaderT (runSS st) gd

askRoutingT = routingTable <$> ask
askRunningOpsT = runningOpsTable <$> ask
askKTree = globalKTree <$> ask
askLocalId = localPeer <$> ask
askLocalStore = localStore <$> ask

readRoutingT = do { rt <- askRoutingT; liftIO . atomically $ readTVar rt }
readRunningOpsT = do { rot <- askRunningOpsT; liftIO . atomically $ readTVar rot }
readKTree = do { kt <- askKTree; liftIO . atomically $ readTVar kt }
readStore = do { s <- askLocalStore; liftIO . atomically $ readTVar s }

modifyTVar :: (a -> a) -> TVar a -> STM ()
modifyTVar f tv = readTVar tv >>= writeTVar tv . f

nrandoms n = do 
  rst <- randoms <$> ask
  liftIO . atomically $ do
    rs <- readTVar rst
    let nrs = take n rs
    writeTVar rst (drop n rs)
    return $ map fromInteger nrs

-- Helper to execute atomically a ST action on a value asked in ServerState
atomicOnAsk :: (MonadIO m) => m a -> (a -> STM b) -> m b
atomicOnAsk asked action = asked >>= liftIO . atomically . action

insertInStore k v = atomicOnAsk askLocalStore (modifyTVar $ M.insert k v)

lookupInStore k = M.lookup k <$> atomicOnAsk askLocalStore readTVar 

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

runningLookup lookupId = M.lookup lookupId <$> readRunningOpsT

runningLookupDone:: Word64 -> ServerState p (Maybe (RunningOps p))
runningLookupDone lookupId = atomicOnAsk askRunningOpsT (\rot -> do
  ro <- readTVar rot
  let val = M.lookup lookupId ro
  case val of
    Nothing -> return Nothing
    Just wr -> modifyTVar (M.delete lookupId) rot >> return val )

newWaitingReply:: p -> KadOp -> Word64 -> Word64 -> ServerState p ()
newWaitingReply peer op msgId opId = 
  atomicOnAsk askRoutingT $ modifyTVar (M.insert msgId $ WaitingReply peer op opId)

waitingReply::(Eq p) => Word64 -> KadOp -> p -> ServerState p (Maybe Word64)
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

newUid:: (ServerState p) Word64
newUid = fromInteger <$> head <$> nrandoms 1

