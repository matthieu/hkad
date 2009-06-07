{-# LANGUAGE NoMonomorphismRestriction #-} 

module KTable
  ( KBucket(..), KTree(..), kinsert, kclosest, ktreeSize,
    kbleaf, binaries, kdepth, nxor, bucketLength, bucketAll, bucketToList
  ) where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.List(nub)
import Data.Word
import Data.Bits

import Debug.Trace

-- Kademlia constants k and b, size of buckets and number of bits in the id space 
-- respectively.
--
kdepth = 20
binaries = 160

-- Data types and helper functions
--
newtype KBucket = KBucket (S.Seq Integer)
  deriving (Show, Eq)

data KTree = KNode { leftKTree:: KTree, rightKTree:: KTree }
            | KLeaf KBucket
  deriving (Show, Eq)

kbleaf = KLeaf . KBucket

bucketInsert (KBucket seq) val = KBucket (seq S.|> val)
bucketElem (KBucket seq) val = F.elem val seq
bucketLength (KBucket seq) = S.length seq
bucketAll fn (KBucket seq) = F.all fn seq
bucketToList (KBucket seq) = F.toList seq

-- Sets common bits to 0 and differing bits to 1
--   ex: 101110 `xor` 100101 = 001011
nxor a b = (a .|. b) `xor` (a .&. b)

-- Inserts the provided node id in the routing table
--
kinsert pivot kt nid = update_closest_bucket kt nid insertOrSplit
  where 
    insertOrSplit pos kb =
      if bucketElem kb nid
        then KLeaf kb
        else if bucketLength kb < kdepth
               then KLeaf $ bucketInsert kb nid
               else if pos == 0 || (nid `nxor` pivot > 2 ^ (pos+1))
                      then KLeaf kb
                      else traverseKTree KNode KNode pos (splitBucket kb pos) nid insertOrSplit

    splitBucket (KBucket seq) pos = pairToNode $ F.foldl separateVals (S.empty,S.empty) seq
      where separateVals (lseq, rseq) v =
              if testBit v pos then (lseq, rseq S.|> v) else (lseq S.|> v, rseq)
            pairToNode (lseq, rseq) = KNode (kbleaf lseq) (kbleaf rseq)

-- Finds at least k nodes closest to the provided id in the routing table. 
--
kclosest kt nid = with_closest_bucket kt nid (returnOrRewind [] nid)
  where
    -- If the closest k bucket has at least k element, we're good we can return
    -- them. Otherwise we have to rewind up the tree to find more nodes.
    returnOrRewind karr mid pos kb =
      if (length newkarr) >= kdepth
        then newkarr
        else rewind newkarr mid pos
      where newkarr = nub $ bucketToList kb ++ karr

    -- Flips the current bit and redo the closest bucket search to explore other
    -- branches. If the bit has already been flipped (comparing to the node we were
    -- originally looking for), we try to flip higher bits instead.
    rewind karr mid pos =
      -- trace ("rewd " ++ show pos) $
      if pos >= binaries
        then karr
        else if (nid .&. 2^pos) `nxor` (mid .&. 2^pos) > 0
               then {-trace ("rew " ++ show pos) $ -} rewind karr mid (pos+1)
               else {-trace ("close " ++ show pos) $ -} with_closest_bucket kt newmid (returnOrRewind karr newmid)
      where newmid = mid `xor` 2^pos

-- Finds the closest bucket to an id and apply the provided transformation 
-- function (from KBucket to KTree) on it.
update_closest_bucket :: (Bits a) => KTree -> a -> (Int -> KBucket -> KTree) -> KTree
update_closest_bucket = traverseKTree KNode KNode (binaries-1)

-- Finds the closest bucket to an id and apply the provided function on it.
--
with_closest_bucket :: (Bits a) => KTree -> a -> (Int -> KBucket -> t) -> t
with_closest_bucket = traverseKTree (flip const) const (binaries-1)

-- Total number of nodes stored in all buckets
--
ktreeSize (KNode left right) = ktreeSize left + ktreeSize right
ktreeSize (KLeaf kb)         = bucketLength kb

-- Goes down the tree by following the provided node id bit track and calls
-- a provided function once a matching bucket has been found.
--
traverseKTree :: (Bits a) => (KTree -> t -> t) -> (t -> KTree -> t) -> Int -> KTree -> a -> (Int -> KBucket -> t) -> t
traverseKTree consRight consLeft pos (KNode left right) nid fn =
  if testBit nid pos 
    then consRight left (traverseKTree consRight consLeft (pos-1) right nid fn)
    else consLeft (traverseKTree consRight consLeft (pos-1) left nid fn) right
traverseKTree _ _ pos kl@(KLeaf kb) nid fn = fn pos kb

