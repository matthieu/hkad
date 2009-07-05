module Kad 
  ( nodeLookup, store, nodeLookupReceive, storeReceive, nodeLookupCallback, valueLookupCallback
  ) where

import Data.Word
import KTable
import Globals

nodeLookup:: Integer -> HandlerFn -> ServerState ()

store:: Integer -> String -> ServerState ()

nodeLookupReceive:: Bool -> Word64 -> Integer -> Peer -> ServerState ()

nodeLookupCallback:: Bool -> Word64 -> Peer -> [Peer] -> ServerState()

valueLookupCallback :: Word64 -> Peer -> String -> ServerState()

storeReceive:: Word64 -> Integer -> String -> Peer -> ServerState ()
