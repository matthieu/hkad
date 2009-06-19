module Comm
  ( KadOp(..), sendLookup, sendLookupReply, parseHeader, localServer, serverDispatch, tunnel,
    toCharArray, fromCharArray
  ) where

import Network.Socket
import Network.BSD
import Control.Concurrent(forkIO)

import Data.Word
import Data.List (genericDrop,unfoldr,intercalate)
import Data.Char(chr,ord)
import Control.Monad(forM, ap, liftM)
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace

import KTable
import Globals
import {-# SOURCE #-} Kad(nodeLookupReceive, nodeLookupCallback)

-- Header: version - 1 byte
--         optype  - 1 byte
--         uid     - 8 bytes 
--
-- Node Lookup: node id - 20 bytes

data PeerHandle = PeerHandle { pSocket :: Socket, pAddress :: SockAddr }

data Header = Header { msgVersion ::Int, msgOp ::KadOp, msgUid ::Word64, sender ::Peer }
  deriving Show

--
-- Client functions to send operations to other nodes.
--

-- Sends to an array of peers a node lookup message for a given node id. The lookup
-- operation itself is tracked by a secondary id stored locally.
sendLookup peers nid lookupId = forM peers (\p -> do 
  msgId <- liftIO newUid
  trace ("sendLookup msgid " ++ show msgId) (return ())
  me    <- askLocalId
  let msg = buildLookupMsg nid msgId me
  newWaitingReply p NodeLookupReplyOp msgId lookupId
  liftIO $ sendToPeer msg p )

  where buildLookupMsg nid msgId me = 
          buildHeader NodeLookupOp msgId me ++ toCharArray nid 160

-- Sends the reply to a node lookup query, sending k nodes and reproducing the
-- received message id.
sendLookupReply peer nodes msgId = do
  me <- askLocalId
  let msg = buildLookupReplyMsg nodes msgId me
  liftIO $ sendToPeer msg peer

  where buildLookupReplyMsg nodes msgId me = 
          buildHeader NodeLookupReplyOp msgId me ++ serPeers nodes

sendToPeer msg peer = do
  phandle <- openPeerHandle (host peer) (port peer)
  sendstr phandle msg
  closePeerHandle phandle

  where sendstr _ [] = return ()
        sendstr phandle omsg = do 
          sent <- sendTo (pSocket phandle) omsg (pAddress phandle)
          sendstr phandle (genericDrop sent omsg)

--
-- Server functions to handle calls coming from other nodes
--

serverDispatch addr msg = do
  let (hdr, rest) = parseHeader msg
  trace (show hdr) (return ())
  let peer = toPeer addr (sender hdr)
  refreshPeer peer
  -- ignoring what we can't handle yet
  if msgVersion hdr /= 1
    then return ()
    else do wait <- waitingReply (msgUid hdr) (msgOp hdr) peer
            trace (show wait) (return ())
            case wait of
              Nothing    -> dispatchOp (msgOp hdr) rest peer (msgUid hdr)
              Just opId  -> dispatchReplyOp (msgOp hdr) opId rest peer

  where dispatchReplyOp NodeLookupReplyOp opId msg peer = do 
          rl <- runningLookup opId
          trace ("peers " ++ msg ++ " " ++ (show $ length msg)) (return ())
          case rl of
            Nothing -> return ()
            Just rl -> nodeLookupCallback peer (if length msg > 0 then deserPeers msg else [])
        dispatchReplyOp _ _ _ _  = return ()

        dispatchOp NodeLookupOp msg peer msgId = nodeLookupReceive (fromCharArray msg) peer msgId
        dispatchOp _ _ _ _                     = return ()

-- implement refresh logic
refreshPeer = insertInKTree

-- TODO handle things like empty messages, adding error handling
parseHeader msg = runState parseHeader' msg
  where parseHeader' = do
          ver  <- parseVersion
          op   <- parseOpType
          uid  <- parseUid
          sndr <- parsePeer
          return $ Header ver op uid sndr

        parseVersion = consuming 1 (ord . head)
        parseOpType  = consuming 1 (\x ->
          case head x of
            '\SOH' -> PingOp
            '\STX' -> PingReplyOp
            '\ETX' -> NodeLookupOp
            '\EOT' -> NodeLookupReplyOp
            _      -> UnknownOp )
        parseUid  = consuming 8 fromCharArray
        parsePeer = do
          str <- get
          let (v,r) = span (/=',') str
          put $ tail r
          return $ deserPeer v

        consuming n fn = do
          str <- get
          let (v, r) = splitAt n str
          put r
          return $ fn v

tunnel :: (SockAddr -> String -> ServerState ()) -> ((SockAddr -> String -> IO ()) -> IO ()) -> ServerState ()
tunnel f k = do
  (rt, rot, kt, lid) <- ask
  liftIO (k (\sock msg -> runServer rt rot kt lid (f sock msg)))

localServer port handlerFn = do
  forkIO . withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    procMessages sock
  return ()

  -- Loops forever processing incoming data
  where procMessages sock = do
          (msg, _, addr) <- recvFrom sock 1024
          handlerFn addr msg
          procMessages sock


-- Utility functions to serialize messages
--

buildHeader:: KadOp -> Word64 -> Peer -> String
buildHeader optype msgId sender = 
  (chr 1) : optypeStr : toCharArray (toInteger msgId) 64 ++ serPeer sender ++ ","
  where optypeStr = case optype of
                      PingOp            -> chr 1
                      PingReplyOp       -> chr 2
                      NodeLookupOp      -> chr 3
                      NodeLookupReplyOp -> chr 4

toCharArray:: Integer -> Int -> [Char]
toCharArray num depth = map (chr . fromInteger) (toBytes num $ toInteger (depth-8))

-- Converts a string to its numeric value by considering that each character is a
-- byte in a n byte number
fromCharArray :: (Num t) => [Char] -> t
fromCharArray str = fst $ foldl (\(acc, exp) ch -> 
  (acc + fromIntegral (ord ch) * 2^exp, exp+8)) (0,0) (reverse str)

-- Serializes peers
serPeers ps = intercalate "," $ map serPeer ps
serPeer p =  (host p) ++ ":" ++ (port p) ++ ":" ++ toCharArray (nodeId p) 160

-- Deserializes peers from message strings using the opposite transformation from ser
deserPeers = map deserPeer . split ','
deserPeer = buildPeer . split ':'
  where buildPeer [h,p,nid] = Peer h p (fromCharArray nid)

split delim s
  | [] == rest = [token]
  | otherwise = token : split delim (tail rest)
  where (token,rest) = span (/=delim) s

-- Decomposes a number into its successive byte values or, said differently, converts
-- a number to base 2^8.
toBytes :: Integer -> Integer -> [Integer]
toBytes num depth = unfoldr byteMod (num,depth)
  where byteMod (num,exp) = let (d,m) = divMod num (2^exp)
                            in if exp < 0 then Nothing else Just (d, (m,exp-8))

-- Build a new peer from packet address and header peer info
toPeer addr pr = let (h,p) = span (/=':') (show addr)
                 in Peer h (port pr) (nodeId pr)

-- Tried below but host is a Word 32...
--   case addr of
--     SockAddrInet port host -> Peer (show host) (show port) (-1)
--     SockAddrInet6 port _ host _ -> Peer (show host) (show port) (-1)
--     SockAddrUnix str -> error $ "Dont know how to translate " ++ str

openPeerHandle hostname port = do
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  return $ PeerHandle sock (addrAddress serveraddr)

closePeerHandle phandle = sClose (pSocket phandle)

