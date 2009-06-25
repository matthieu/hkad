module Kad 
  ( startNode, nodeLookup, nodeLookupReceive, nodeLookupCallback
  ) where

import qualified Data.Map as M
import Data.Word
import Data.List(sortBy, (\\), delete, nub)
import Data.Bits

import Control.Monad(liftM, liftM2, liftM3, forM)
import Control.Concurrent.STM
import Control.Monad.Trans(liftIO)

import System.Random
import System.Log.Logger
import Debug.Trace

import KTable
import Globals
import Comm

-- Global parallelization constant
alpha = 3

-- Joins the network, knowing one existing peer, by doing a lookup on this
-- node's id and refreshing all newly created buckets.
startNode peer =
  if (read (port peer) > 0)
    then do
      insertInKTree peer
      me <- askLocalId
      -- Node lookup on local id
      nodeLookup $ nodeId me
      -- Node refresh on all buckets
      kt <- readKTree
      ids <- liftIO . mapM randomRIO $ kbucketsRange kt
      trace (">> "++show (length ids)) (return ())
      forM ids nodeLookup
      return ()
    else return ()

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
  ktree <- readKTree
  let close = kclosest ktree nid

  debug $ "Replying to node lookup for nid " ++ show nid ++ " from peer " ++ show peer ++ " with nodes " ++ show close
  sendLookupReply peer close msgId

-- Received the result of a node lookup request, stores the k nodes received
-- and updates the status of the node lookup, initiates a new alpha lookup
-- round or completes.
-- TODO timer
-- TODO alpha round fails to find closer node
-- TODO lookup should be read and written in the same atomically
nodeLookupCallback:: Word64 -> Peer -> [Peer] -> ServerState()
nodeLookupCallback opId peer nodes = do
  debug $ "Received a node lookup callback from peer " ++ show peer ++ " with nodes " ++ show nodes
  me  <- askLocalId
  rlm <- runningLookup opId
  case rlm of
    Nothing -> debug ("Unknown lookup id: " ++ show opId) >> return ()
    Just rl -> let fnodes = delete me nodes
                   rest = delete peer $ pending rl
                   nk   = nub $ known rl ++ fnodes
                   nq   = nub $ peer : queried rl
               in do
      -- If we finished an alpha round, initiating a new one or terminating, otherwise
      -- just updating data
      if null $ rest
        then let nc = closestNodes (lookupNodeId rl) nk \\ nq
                 na = pickAlphaNodes nc
             in if null nc
                  then do debug $ "Done! Closest nodes: " ++ show (take kdepth nk) -- TODO cleanup
                  else do
                    newRunningLookup (lookupNodeId rl) nk na nq opId
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
      d2 = nodeId p2 `xor` pivot
  in compare d1 d2

-- K nodes closest to the pivot by the xor metric
closestNodes :: Integer -> [Peer] -> [Peer]
closestNodes pivot = take kdepth . sortBy (closestNode pivot)

debug s = liftM ((++ " " ++ s) . show . nodeId) askLocalId >>= liftIO . debugM "Kad"

