module Kad 
  ( nodeLookup, nodeLookupReceive, nodeLookupCallback
  ) where

import qualified Data.Map as M
import Data.Word
import Data.List(sortBy, (\\), delete)
import Data.Bits

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

-- Initiates a node lookup on the network
nodeLookup:: Integer -> ServerState ()
nodeLookup nid = do
  debug $ "Starting a node lookup for node " ++ show nid
  uid   <- liftIO newUid
  ktree <- readKTree

  -- k closest elements in the k tree
  let kc = kclosest ktree nid
  if null kc
    then return ()
    else do
      let ps = pickAlphaNodes kc
      -- Initially we know about the k nodes, query the first alpha and haven't 
      -- queried anything yet
      newRunningLookup nid kc ps [] uid
      -- sending a lookup on the alpha nodes picked
      sendLookup ps nid uid

-- Received a node lookup request, queries the KTable for the k closest and
-- sens the information back.
nodeLookupReceive:: Word64 -> Integer -> Peer -> ServerState ()
nodeLookupReceive msgId nid peer = do
  debug $ "Received a node lookup from peer " ++ show peer ++ " for nid " ++ show nid
  ktree <- readKTree
  let close = kclosest ktree nid

  debug $ "Replying to node lookup for nid " ++ show nid ++ " from peer " ++ show peer ++ " with nodes " ++ show close
  sendLookupReply peer close msgId

-- Received the result of a node lookup request, stores the k nodes received
-- and updates the status of the node lookup, initiates a new alpha lookup
-- round or completes.
-- TODO timer
-- TODO alpha round fails to find closer node
nodeLookupCallback:: Word64 -> Peer -> [Peer] -> ServerState()
nodeLookupCallback opId peer nodes = do
  debug $ "Received a node lookup callback from peer " ++ show peer ++ " with nodes " ++ show nodes
  rlm <- runningLookup opId
  case rlm of
    Nothing -> debug ("Unknown lookup id: " ++ show opId) >> return ()
    Just rl -> let rest = delete peer $ pending rl
                   nk   = known rl ++ nodes
                   nq   = peer : queried rl
               in do
      -- If we finished an alpha round, initiating a new one or terminating, otherwise
      -- just updating data
      if null $ rest
        then let nc = closestNodes (lookupNodeId rl) (nk \\ nq)
                 na = pickAlphaNodes nc
             in if null nc
                  then debug $ "Done! Closest nodes: " ++ show (take kdepth nk) -- TODO cleanup
                  else do
                    newRunningLookup (lookupNodeId rl) nk nc nq opId
                    sendLookup na (lookupNodeId rl) opId
        else newRunningLookup (lookupNodeId rl) nk rest nq opId

-- Pick the "best" alpha nodes out of the k selected for lookup. For now we only
-- select the 3 last ones (most stable) but it would be the right place to plug 
-- in Vivaldi coordinates.
pickAlphaNodes kc =  
  let idx = length kc - alpha
  in snd $ splitAt idx kc

closestNode pivot p1 p2 = 
  let d1 = nodeId p1 `xor` pivot
      d2 = nodeId p1 `xor` pivot
  in compare d1 d2

-- K nodes closest to the pivot by the xor metric
closestNodes :: Integer -> [Peer] -> [Peer]
closestNodes pivot = take kdepth . sortBy (closestNode pivot)

