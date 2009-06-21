module Kad 
  ( nodeLookup, nodeLookupReceive, nodeLookupCallback
  ) where

import Data.Word
import KTable
import Globals

nodeLookupReceive:: Word64 -> Integer -> Peer -> ServerState ()

nodeLookupCallback:: Word64 -> Peer -> [Peer] -> ServerState()

nodeLookup:: Integer -> ServerState ()
