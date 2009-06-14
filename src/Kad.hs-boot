module Kad 
  ( nodeLookup, nodeLookupReceive, nodeLookupCallback
  ) where

import Data.Word
import KTable
import Globals

nodeLookupReceive:: Integer -> Peer -> Word64 -> ServerState ()

nodeLookupCallback:: Peer -> [Peer] -> ServerState()

nodeLookup:: Integer -> ServerState ()
