module Comm
  ( KadOp(..), sendLookup, newUid, parseHeader,
  ) where

import Network.Socket
import Network.BSD

import Data.Word
import Data.List (genericDrop,unfoldr)
import Data.Char(chr,ord)
import Control.Monad(forM, ap, liftM)
import Control.Monad.State

import KTable
import Globals

-- Header: version - 1 byte
--         optype  - 1 byte
--         uid     - 8 bytes 
--
-- Node Lookup: node id - 20 bytes

data PeerHandle = PeerHandle { pSocket :: Socket, pAddress :: SockAddr }

-- Client functions to send operations to other nodes.
--

sendLookup peers nid lookupId = forM peers (\p -> do 
  msgId <- liftIO newUid
  msg   <- buildLookupMsg nid msgId
  newWaitingReply p NodeLookupOp msgId lookupId
  liftIO $ sendToPeer msg p )

  where buildLookupMsg nid msgId = liftM (++ toCharArray nid 160) $ buildHeader NodeLookupOp msgId

sendToPeer msg peer = do
  phandle <- openPeerHandle (host peer) (port peer)
  sendstr phandle msg
  closePeerHandle phandle

  where sendstr _ [] = return ()
        sendstr phandle omsg = do 
          sent <- sendTo (pSocket phandle) omsg (pAddress phandle)
          sendstr phandle (genericDrop sent omsg)

-- Server functions to handle calls coming from other nodes
--

serverDispatch addr msg = do
  let (hdr, rest) = parseHeader msg
  -- ignoring what we can't handle
  if msgVersion hdr /= 1
    then return ()
    else do wait <- waitingReply (msgUid hdr) (msgOp hdr) (toPeer addr)
            case wait of
              Nothing -> return ()
              opId    -> dispatchOp (msgOp hdr) opId rest

  where dispatchOp NodeLookupOp opId msg = do 
          rl <- runningLookup opId
          case rl of
            Nothing -> return ()
            Just rl -> return ()
        dispatchOp _ _ _  = return ()

data Header = Header { msgVersion ::Int, msgOp ::KadOp, msgUid ::Word64 }
  deriving Show

-- TODO handle things like empty messages, adding error handling
parseHeader msg = runState parseHeader' msg
  where parseHeader' = do
          ver  <- parseVersion
          op   <- parseOpType
          uid  <- parseUid
          return $ Header ver op uid

        parseVersion = consuming 1 (ord . head)
        parseOpType = consuming 1 (\x ->
          case head x of
            '\SOH' -> PingOp
            '\STX' -> NodeLookupOp
            _      -> UnknownOp )
        parseUid = consuming 8 fromCharArray

        consuming n fn = do
          str <- get
          let (v, r) = splitAt n str
          put r
          return $ fn v

localServer port handlerFn = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  procMessages sock

  -- Loops forever processing incoming data
  where procMessages sock = do
          (msg, _, addr) <- recvFrom sock 1024
          handlerFn addr msg
          procMessages sock
                  
-- Utility functions to serialize messages
--

buildHeader optype msgId = do
  return $ buildHeader' msgId optype

  where buildHeader':: (Integral a) => a -> KadOp -> String
        buildHeader' uid optype = (chr 1) : optypeStr : toCharArray uid 64
        optypeStr = case optype of
                      PingOp       -> chr 1
                      NodeLookupOp -> chr 2

toCharArray:: (Integral a) => a -> Int -> [Char]
toCharArray num depth = map chr (toBytes num depth)

-- Converts a string to its numeric value by considering that each character is a
-- byte in a n byte number
fromCharArray :: (Num t) => [Char] -> t
fromCharArray str = fst $ foldl (\(acc, exp) ch -> 
  (acc + fromIntegral (ord ch) * 2^exp, exp+1)) (0,0) (reverse str)

-- Decomposes a number into its successive byte values or, said differently, converts
-- a number to base 2^8.
toBytes :: (Integral t, Integral a) => a -> t -> [Int]
toBytes num depth = unfoldr byteMod (num,depth)
  where byteMod (num,exp) = let (d,m) = divMod num (2^exp)
                            in if exp < 0 then Nothing else Just (fromIntegral d ::Int, (m,exp-8))

toPeer addr = 
  case addr of
    SockAddrInet port host -> Peer (show host) (show port) (-1)
    SockAddrInet6 port _ host _ -> Peer (show host) (show port) (-1)
    SockAddrUnix str -> error $ "Dont know how to translate " ++ str

openPeerHandle hostname port = do
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  return $ PeerHandle sock (addrAddress serveraddr)

closePeerHandle phandle = sClose (pSocket phandle)

-- main = do
--   params <- getArgs
--   if head params == "client"
--     then do    
--       ph <- openPeerHandle "localhost" "10000"
--       sendPing ph
--       closePeerHandle ph
--     else localServer "10000" (\addr msg -> putStrLn $ msg ++ " / " ++ (show addr))
