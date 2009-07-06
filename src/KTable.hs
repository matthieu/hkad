{-# LANGUAGE NoMonomorphismRestriction #-} 

module KTable
  ( KTree, FKTree(..), Node(..), kinsert, kclosest, kbucketsRange, ktreeSize, ktreeList,
    kbleaf, binaries, kdepth, nxor
  ) where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.List(nub)
import Data.Word
import Data.Bits
import Data.Monoid

import Debug.Trace

-- Kademlia constants k and b, size of buckets and number of bits in the id space 
-- respectively.
--
kdepth = 20
binaries = 160

-- Data types and helper functions
--

class Node a where
  nodeId :: a -> Integer
  
type KBucket a = S.Seq a

data FKTree a = KNode { leftKTree:: FKTree a, rightKTree:: FKTree a}
                | KLeaf a
  deriving (Show, Eq)

instance F.Foldable FKTree where
  foldMap f (KLeaf x)   = f x
  foldMap f (KNode l r) = F.foldMap f l `mappend` F.foldMap f r

type KTree a = FKTree (KBucket a)

kbleaf = KLeaf

-- Sets common bits to 0 and differing bits to 1
--   ex: 101110 `xor` 100101 = 001011
nxor a b = (a .|. b) `xor` (a .&. b)

-- Inserts the provided peer in the node tree
--
kinsert pivot kt peer = update_closest_bucket kt nid insertOrSplit
  where 
    insertOrSplit pos kb =
      if peer `F.elem` kb
        then KLeaf kb
        else if S.length kb < kdepth
               then KLeaf $ kb S.|> peer
               else if pos == 0 || (nid `nxor` pivot > 2 ^ (pos+1))
                      -- TODO before dropping it, check that it wouldn't be a good replacement for one of the existing bucket node
                      then KLeaf kb 
                      else traverseKTree KNode KNode pos (splitBucket kb pos) nid insertOrSplit

    -- TODO logic for highly unbalanced trees
    splitBucket seq pos = pairToNode $ F.foldl separateVals (S.empty,S.empty) seq
      where separateVals (lseq, rseq) p =
              if testBit (nodeId p) pos then (lseq, rseq S.|> p) else (lseq S.|> p, rseq)
            pairToNode (lseq, rseq) = KNode (kbleaf lseq) (kbleaf rseq)

    nid = nodeId peer

-- Finds at least k nodes closest to the provided id in the node tree
--
kclosest kt nid = with_closest_bucket kt nid (returnOrRewind [] nid)
  where
    -- If the closest k bucket has at least k element, we're good we can return
    -- them. Otherwise we have to rewind up the tree to find more nodes.
    returnOrRewind karr mid pos kb =
      if (length newkarr) >= kdepth
        then newkarr
        else rewind newkarr mid pos
      where newkarr = nub $ F.toList kb ++ karr

    -- Flips the current bit and redo the closest bucket search to explore other
    -- branches. If the bit has already been flipped (comparing to the node we were
    -- originally looking for), we try to flip higher bits instead.
    rewind karr mid pos =
      if pos >= binaries
        then karr
        else if (nid .&. 2^pos) `nxor` (mid .&. 2^pos) > 0
               then rewind karr mid (pos+1)
               else with_closest_bucket kt newmid (returnOrRewind karr newmid)
      where newmid = mid `xor` 2^pos

-- Range of values within each bucket
kbucketsRange = F.foldr (\kb r -> (nodeId . F.minimum $ kb, nodeId . F.maximum $ kb) : r) []

-- Finds the closest bucket to an id and apply the provided transformation 
-- function (from KBucket to KTree) on it.
update_closest_bucket :: (Bits a) => KTree p -> a -> (Int -> KBucket p -> KTree p) -> KTree p
update_closest_bucket = traverseKTree KNode KNode (binaries-1)

-- Finds the closest bucket to an id and apply the provided function on it.
--
with_closest_bucket :: (Bits a) => KTree p -> a -> (Int -> KBucket p -> t) -> t
with_closest_bucket = traverseKTree (flip const) const (binaries-1)

-- Total number of nodes stored in all buckets
--
ktreeSize = F.foldr ((+) . S.length) 0
ktreeList = F.foldr ((++) . F.toList) []

-- Goes down the tree by following the provided node id bit track and calls
-- a provided function once a matching bucket has been found.
--
traverseKTree :: (Bits a) => (KTree p -> t -> t) -> (t -> KTree p -> t) -> 
                              Int -> KTree p -> a -> (Int -> KBucket p -> t) -> t
traverseKTree consRight consLeft pos (KNode left right) nid fn =
  if testBit nid pos 
    then consRight left (traverseKTree consRight consLeft (pos-1) right nid fn)
    else consLeft (traverseKTree consRight consLeft (pos-1) left nid fn) right
traverseKTree _ _ pos kl@(KLeaf kb) nid fn = fn pos kb

