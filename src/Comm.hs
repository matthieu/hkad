module Comm
  ( IPPeer(..), parseHeader, localServer, serverDispatch, tunnel,
    buildHeader, toCharArray, fromCharArray
  ) where

import Network.Socket
import Network.BSD
import Control.Concurrent(forkIO)

import Data.Maybe(listToMaybe)
import Data.Word
import Data.List (genericDrop,unfoldr,intercalate)
import Data.Char(chr,ord)
import Data.Bits

import Control.Arrow(second)
import Control.Monad(forM, ap, liftM)
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace

import KTable
import Globals
import Kad -- (storeReceive, nodeLookupReceive, nodeLookupCallback, valueLookupCallback)

-- TODO shut out nodes that are too chatty
-- Header: version - 1 byte
--         optype  - 1 byte
--         uid     - 8 bytes 
--
-- Node Lookup: node id - 20 bytes

data IPPeer = IPPeer { host:: String, port:: String, ipNodeId:: Integer }

instance Eq IPPeer where
  p1 == p2 = if nodeId p1 /= -1 && nodeId p2 /= -1 
               then nodeId p1 == nodeId p2
               else host p1 == host p2 && port p1 == port p2

instance Show IPPeer where
  show p = "< " ++ host p ++ " / " ++ port p ++ " / " ++ show (nodeId p) ++ " >"

instance Ord IPPeer where
  compare p1 p2 = compare (nodeId p1) (nodeId p2)

instance Peer IPPeer where
  sendLookup = sendLookupIP
  sendLookupReply = sendLookupReplyIP
  sendStore = sendStoreIP
  sendValueReply = sendValueReplyIP
  sendToPeer = sendToPeerIP
  serPeer p = (serIP $ host p) ++ (serPort $ port p) ++ toCharArray (nodeId p) 20

deserIP = intercalate "." . map (show . ord)
serIP = map (chr . read) . split '.' 
deserPort = (show ::Int -> String) . fromCharArray
serPort = (flip toCharArray 2) . read

instance Node IPPeer where
  nodeId = ipNodeId

data PeerHandle = PeerHandle { pSocket :: Socket, pAddress :: SockAddr }

data Header = Header { msgVersion ::Int, msgOp ::KadOp, msgUid ::Word64, sender ::IPPeer }
  deriving Show

--
-- Client functions to send operations to other nodes.
--

-- Sends to an array of peers a lookup message for a given node id. The lookup
-- operation itself is tracked by a secondary id stored locally. Supports both
-- node and value lookup using the last boolean
sendLookupIP peers nid lookupId valL = forM peers (\p -> do 
  msgId <- newUid
  me    <- askLocalId
  let msg = buildHeader (if valL then ValueLookupOp else NodeLookupOp) msgId me ++ toCharArray nid 20

  newWaitingReply p (if valL then ValueLookupReplyOp else NodeLookupReplyOp) msgId lookupId
  liftIO $ sendToPeerIP msg p ) >> return ()

-- Sends the reply to a node lookup query, sending k nodes and reproducing the
-- received message id.
sendLookupReplyIP peer nodes msgId valL = do
  me <- askLocalId
  let msg = buildHeader (if valL then ValueLookupReplyOp else NodeLookupReplyOp) msgId me ++ serPeers nodes
  liftIO $ sendToPeerIP msg peer

sendValueReplyIP peer val msgId = do
  me <- askLocalId
  let msg = buildHeader ValueLookupReplyOp msgId me ++ 
        if length val `mod` 26 == 0 then val ++ " " else val
  liftIO $ sendToPeerIP msg peer

sendStoreIP peers key value storeId = forM peers (\p -> do
  msgId <- newUid
  me    <- askLocalId
  let msg = buildHeader StoreOp msgId me ++ toCharArray key 20 ++ value

  newWaitingReply p StoreReplyOp msgId storeId
  liftIO $ sendToPeerIP msg p ) >> return ()

sendToPeerIP msg peer = do
  phandle <- openPeerHandle (host peer) (port peer)
  sendstr phandle msg
  closePeerHandle phandle
  return ()

  where sendstr _ [] = return ()
        sendstr phandle omsg = do 
          sent <- sendTo (pSocket phandle) omsg (pAddress phandle)
          sendstr phandle (genericDrop sent omsg)

--
-- Server functions to handle calls coming from other nodes
--

serverDispatch addr msg = do
  let (hdr, rest) = parseHeader msg
  let peer = toPeer addr (sender hdr)
  refreshPeer peer
  -- ignoring what we can't handle yet
  if msgVersion hdr /= 1
    then return ()
    else do wait <- waitingReply (msgUid hdr) (msgOp hdr) peer
            case wait of
              Nothing    -> dispatchOp (msgOp hdr) rest peer (msgUid hdr)
              Just opId  -> dispatchReplyOp (msgOp hdr) opId rest peer

  where dispatchReplyOp NodeLookupReplyOp opId msg peer = do 
          rl <- runningLookup opId
          case rl of
            Nothing -> return ()
            Just rl -> nodeLookupCallback False opId peer (if length msg > 0 then deserPeers msg else [])
        dispatchReplyOp ValueLookupReplyOp opId msg peer = do 
          rl <- runningLookup opId
          case rl of
            Nothing -> return ()
            Just rl -> 
              if length msg `mod` 26 == 0 
                then nodeLookupCallback True opId peer (if length msg > 0 then deserPeers msg else [])
                else valueLookupCallback opId peer msg
        dispatchReplyOp _ _ _ _  = return ()

        dispatchOp NodeLookupOp msg peer msgId = 
          nodeLookupReceive False msgId (fromCharArray msg) peer
        dispatchOp StoreOp msg peer msgId = 
          storeReceive msgId (fromCharArray $ take 20 msg) (drop 20 msg) peer
        dispatchOp ValueLookupOp msg peer msgId =
          nodeLookupReceive True msgId (fromCharArray msg) peer
        dispatchOp _ _ _ _                = return ()

-- TODO implement refresh logic
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
            '\ENQ' -> StoreOp
            '\ACK' -> StoreReplyOp
            '\a'   -> ValueLookupOp
            '\b'   -> ValueLookupReplyOp
            _      -> UnknownOp )
        parseUid  = consuming 8 fromCharArray
        parsePeer = consuming 26 deserPeer

        consuming n fn = do
          str <- get
          let (v, r) = splitAt n str
          put r
          return $ fn v

-- Deserializes peers from message strings using the opposite transformation from ser
deserPeers = map deserPeer . splitEvery 26
deserPeer s = 
  let (h,rest) = splitAt 4 s
      (p,nid)  = splitAt 2 rest
  in IPPeer (deserIP h) (deserPort p) (fromCharArray nid)

split delim = unfoldr (\b -> fmap (const . (second $ drop 1) . break (==delim) $ b) . listToMaybe $ b)
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

tunnel :: (SockAddr -> String -> ServerState p ()) -> ((SockAddr -> String -> IO ()) -> IO ()) -> ServerState p ()
tunnel f k = do
  gs <- ask
  liftIO (k (\sock msg -> runServer gs (f sock msg)))

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

-- Build a new peer from packet address and header peer info
toPeer addr pr = let (h,p) = span (/=':') (show addr)
                 in IPPeer h (port pr) (nodeId pr)


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

