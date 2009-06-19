module Kad 
  ( nodeLookup, nodeLookupReceive, nodeLookupCallback
  ) where

import qualified Data.Map as M
import Data.Word
import Control.Monad(liftM, liftM2, liftM3)
import Control.Concurrent.STM
import Control.Monad.Trans(liftIO)
import System.Log.Logger
import Debug.Trace

import KTable
import Globals
import Comm

-- TODO shut out nodes that are too chatty

alpha = 3

debug = liftIO . debugM "Kad"

nodeLookup:: Integer -> ServerState ()
nodeLookup nid = do
  debug $ "Starting a node lookup for node " ++ show nid
  uid   <- liftIO newUid
  ktree <- readKTree
  let kc = kclosest ktree nid
  if null kc
    then return ()
    else do
      let (rs, ps) = pickAlphaNodes kc
      newRunningLookup rs ps uid
      sendLookup ps nid uid
      return ()

-- Received a node lookup request, queries the KTable for the k closest and
-- sens the information back.
nodeLookupReceive:: Integer -> Peer -> Word64 -> ServerState ()
nodeLookupReceive nid peer msgId = do
  debug $ "Received a node lookup from peer " ++ show peer ++ " for nid " ++ show nid
  ktree <- readKTree
  let close = kclosest ktree nid
  debug $ "Replying to node lookup for nid " ++ show nid ++ " from peer " ++ show peer ++ " with nodes " ++ show close
  sendLookupReply peer close msgId

-- Received the result of a node lookup request, stores the k nodes received
-- and updates the status of the node lookup.
nodeLookupCallback:: Peer -> [Peer] -> ServerState()
nodeLookupCallback peer nodes = do
  debug $ "Received a node lookup callback from peer " ++ show peer ++ " with nodes " ++ show nodes
  return ()

-- Pick the "best" alpha nodes out of the k selected for lookup. For now we only
-- select the 3 last ones but it would be the right place to plug in Vivaldi
-- coordinates.
--
pickAlphaNodes kc = 
  let idx = length kc - alpha
  in case idx of
       idx | idx > 0  -> splitAt idx kc
           | idx <= 0 -> ([], kc)

