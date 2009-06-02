module KTable
  ( KBucket(..), KTree(..), kinsert,
    kbleaf, binaries, kdepth, nxor, bucketLength, bucketAll
  ) where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Word
import Data.Bits

import Debug.Trace

kdepth = 20
binaries = 160

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

kinsert pivot kt val = kinsert' pivot kt val (binaries-1)

kinsert' pivot kl@(KLeaf kb) val pos = 
  if bucketLength kb < kdepth
    then KLeaf $ bucketInsert kb val
    else if pos == 0 || (val `nxor` pivot > 2 ^ (pos+1)) || bucketElem kb val
           then kl 
           else kinsert' pivot (splitBucket kb pos) val pos

kinsert' pivot (KNode left right) val pos = 
  if testBit val pos 
    then KNode left (kinsert' pivot right val (pos-1))
    else KNode (kinsert' pivot left val (pos-1)) right

splitBucket (KBucket seq) pos = pairToNode $ F.foldl separateVals (S.empty,S.empty) seq
  where separateVals (lseq, rseq) v =
          if testBit v pos then (lseq, rseq S.|> v) else (lseq S.|> v, rseq)
        pairToNode (lseq, rseq) = KNode (kbleaf lseq) (kbleaf rseq)

nxor a b = (a .|. b) `xor` (a .&. b)

