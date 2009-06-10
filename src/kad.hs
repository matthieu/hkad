
module Kad 
  ( nodeLookup    
  ) where

import qualified Data.Map as M
import Data.Word
import Control.Monad(liftM, liftM2, liftM3)
import Control.Concurrent.STM
import Control.Monad.Trans(liftIO)

import KTable
import Globals
import Comm

-- TODO shut out nodes that are too chatty

alpha = 3

nodeLookup:: Integer -> ServerState ()
nodeLookup nid = do
  uid    <- liftIO newUid
  ktree <- readKTree
  let kc = kclosest ktree nid
  let (rs, ps) = pickAlphaNodes kc
  newRunningLookup rs ps uid
  sendLookup ps nid uid
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

