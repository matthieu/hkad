{-# LANGUAGE NoMonomorphismRestriction #-} 

module Kad 
  ( startNode, nodeLookup, valueLookup, store, 
    nodeLookupReceive, storeReceive, nodeLookupCallback, valueLookupCallback,
    fromCharArray, toCharArray, buildHeader, serPeers
  ) where

import Prelude hiding (error)
import qualified Data.Map as M
import Data.Word
import Data.Maybe
import Data.List(sortBy, (\\), delete, nub)
import Data.Bits
import Data.Char(chr,ord)
import Data.List (genericDrop,unfoldr,intercalate)

import Control.Monad(liftM, liftM2, liftM3, forM)
import Control.Concurrent.STM
import Control.Monad.Trans(liftIO)
import Control.Arrow(second)

import System.Random
import System.Log.Logger
import Debug.Trace

import KTable
import Globals

-- Global parallelization constant
alpha = 3

-- Joins the network, knowing one existing peer, by doing a lookup on this
-- node's id and refreshing all newly created buckets.
startNode peer = do
  insertInKTree peer
  me <- askLocalId
  -- Node lookup on local id
  nodeLookup (nodeId me) $ PeersHandler (\peers -> do
    -- Node refresh on all buckets
    kt <- readKTree
    ids <- liftIO . mapM randomRIO $ kbucketsRange kt
    forM ids (flip nodeLookup . PeersHandler $ const (return ()))
    return () )

-- Initiates a node lookup on the network. Starts with the k closest
-- nodes known locally and gets closer using the nodes returned.
nodeLookup = genericLookup False

-- Value lookup: pretty much like a node lookup except that if a node has the
-- value in its local store, it returns it instead of the k closest nodes.
valueLookup = genericLookup True

genericLookup valL nid handlerFn = do
  debug $ "Starting a node lookup for node " ++ show nid
  uid   <- newUid
  ktree <- readKTree

  -- k closest elements in the k tree
  let kc = kclosest ktree nid
  if null kc
    then return ()
    else do
      let ps = pickAlphaNodes kc
      -- Initially we know about the k nodes, query the first alpha and haven't 
      -- queried anything yet
      newRunningLookup nid kc ps [] uid handlerFn
      -- sending a lookup on the alpha nodes picked
      sendLookup ps nid uid valL

genericLookup2 valL nid handlerFn = do
  debug $ "Starting a node lookup for node " ++ show nid
  uids <- nrandoms (alpha + 1)
  let (lookupId, msgIds) = splitAt 1 uids
  ktree <- readKTree
  me    <- askLocalId
  routeT <- readRoutingT
  opsT <- readRunningOpsT

  let (msgs, nopsT, nrouteT) = prepareLookups valL nid handlerFn opsT routeT me ktree (head lookupId) msgIds
  -- TODO update maps
  liftIO . mapM (uncurry sendToPeer) $ msgs

prepareLookups valL nid handlerFn opsT routeT me ktree lookupId msgIds = (mps, newOpsT, newRouteT)  
  where (mps, newRouteT) = foldr (\(mid,p) acc -> 
          comb (prepareLookup valL nid p routeT me lookupId mid) acc) ([], routeT) (zip msgIds ps)
        comb (mp, rt) (arr, ort) = (mp:arr, rt)
        kc = kclosest ktree nid
        ps = pickAlphaNodes kc
        newOpsT = M.insert lookupId (RunningLookup nid kc ps [] handlerFn) opsT

prepareLookup valL nid peer routeT me lookupId msgId = ((msg, peer), newRouteT)
  where msg = buildHeader lookpOp msgId me ++ toCharArray nid 20
        newRouteT = M.insert msgId (WaitingReply peer replyOp lookupId) routeT
        lookpOp = if valL then ValueLookupOp else NodeLookupOp
        replyOp = if valL then ValueLookupReplyOp else NodeLookupReplyOp

-- Stores a key / value pair by doing a node lookup and sending a store command
-- to the closest nodes found
store key kdata = nodeLookup key $ PeersHandler (\peers -> (liftIO . putStrLn $ "Sending store to peers " ++ show peers) >> newUid >>= sendStore peers key kdata)

-- Received a node lookup request, queries the KTable for the k closest and
-- sends the information back.
nodeLookupReceive valL msgId nid peer = do
  localVal <- lookupInStore nid
  if valL && isJust localVal
    then do
      liftIO . putStrLn $ "Found local value asked by " ++ show peer
      sendValueReply peer (fromJust localVal) msgId
    else do
      ktree <- readKTree
      let close = kclosest ktree nid
      debug $ "Replying to node lookup for nid " ++ show nid ++ " from peer " ++ show peer ++ " with nodes " ++ show close
      sendLookupReply peer close msgId valL

-- Received the result of a lookup request, stores the k nodes received
-- and updates the status of the node lookup, initiates a new alpha lookup
-- round or completes. Supports both node and value lookup using the first
-- boolean.
-- TODO timer
-- TODO alpha round fails to find closer node
-- TODO lookup should be read and written in the same atomically
nodeLookupCallback valL opId peer nodes = do
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
                  then do 
                    debug $ "Done! Closest nodes: " ++ show (take kdepth nk)
                    rld <- runningLookupDone opId
                    case rld of
                      Nothing -> error $ "Couldn't find lookup " ++ (show opId) ++ " for deletion."
                      Just _  -> case lookupHandler rl of
                                (PeersHandler fn)   -> fn $ take kdepth nk
                                (ContentHandler fn) -> fn Nothing -- Value lookup didn't find anything
                  else do
                    newRunningLookup (lookupNodeId rl) nk na nq opId (lookupHandler rl)
                    sendLookup na (lookupNodeId rl) opId valL
        else newRunningLookup (lookupNodeId rl) nk rest nq opId (lookupHandler rl)

-- Received the result of a value lookup callback, the string received is the
-- expected value.
valueLookupCallback opId peer val = do
  rld <- runningLookupDone opId
  case rld of
    Nothing -> error $ "Couldn't find lookup " ++ (show opId) ++ " for deletion."
    Just rl -> case lookupHandler rl of
              (ContentHandler fn) -> fn $ Just val
              _ -> error "Wrong handler for value lookup!"

-- Responds to a store call by insert the key/value received in the local storage.
-- TODO peer could also respond with 'store full', what do we do?
storeReceive msgId key val peer = (liftIO . putStrLn $ "local store for " ++ show key ++ " " ++ show val) >> insertInStore key val

-- Pick the "best" alpha nodes out of the k selected for lookup. For now we only
-- select the 3 last ones (most stable) but it would be the right place to plug 
-- in Vivaldi coordinates.
pickAlphaNodes kc =  
  let idx = length kc - alpha
  in drop idx kc

closestNode pivot p1 p2 = 
  let d1 = nodeId p1 `xor` pivot
      d2 = nodeId p2 `xor` pivot
  in compare d1 d2

-- K nodes closest to the pivot by the xor metric
closestNodes pivot = take kdepth . sortBy (closestNode pivot)

debug s = liftM ((++ " " ++ s) . show . nodeId) askLocalId >>= liftIO . debugM "Kad"
error s = liftM ((++ " " ++ s) . show . nodeId) askLocalId >>= liftIO . errorM "Kad"

-- Utility functions to serialize messages
--

buildHeader optype msgId sender = 
  (chr 1) : optypeStr : toCharArray (toInteger msgId) 8 ++ serPeer sender
  where optypeStr = case optype of
                      PingOp              -> chr 1
                      PingReplyOp         -> chr 2
                      NodeLookupOp        -> chr 3
                      NodeLookupReplyOp   -> chr 4
                      StoreOp             -> chr 5
                      StoreReplyOp        -> chr 6
                      ValueLookupOp       -> chr 7
                      ValueLookupReplyOp  -> chr 8

-- Convert an integer to a string of n bytes
toCharArray:: Integer -> Int -> [Char]
toCharArray num depth = map (chr . fromInteger) (toBytes num $ toInteger depth)

-- Converts a string to its numeric value by considering that each character is a
-- byte in a n byte number
fromCharArray :: (Bits a) => [Char] -> a
fromCharArray str = foldl (\acc ch -> shift acc 8 + fromIntegral (ord ch)) 0 str

-- Serializes peers
serPeers  = concat . map serPeer

-- Decomposes a number into its successive byte values or, said differently, converts
-- a number to base 2^8.
toBytes :: Integer -> Integer -> [Integer]
toBytes num depth = unfoldr byteMod (num,depth-1)
  where byteMod (num,exp) = let (d,m) = divMod num (2^(exp*8))
                            in if exp < 0 then Nothing else Just (d, (m,exp-1))

