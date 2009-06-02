import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Word
import Data.Bits
import Data.List(nub)

import Debug.Trace
import Test.QuickCheck

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
--  trace (show val) $
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

depth (KNode left right) d = max (depth left (d+1)) (depth right (d+1))
depth (KLeaf kb) d = d

nxor a b = (a .|. b) `xor` (a .&. b)

--
-- Quickcheck
--

instance Arbitrary KTree where
  arbitrary = do
    ks <- choose (1,2000) :: Gen Int
    case ks of
      1 -> return $ kbleaf S.empty
      otherwise -> do t <- arbitrary
                      v <- choose (0, 2^binaries-1) :: Gen Integer
                      return $ kinsert 0 t v

-- Checks that all buckets have k or less elements
prop_max_k_bucket (KNode l r) = prop_max_k_bucket l && prop_max_k_bucket r
prop_max_k_bucket (KLeaf b) = bucketLength b <= kdepth

-- Checks that all bucket element have the right bit sequence with its tree position
prop_all_proper_bits (KLeaf b) = True
prop_all_proper_bits kn@(KNode _ _) = prop_all_proper_bits' kn 0 binaries
  where prop_all_proper_bits' (KNode l r) prefix idx = 
          prop_all_proper_bits' l prefix (idx-1) 
                   && prop_all_proper_bits' r (prefix+2^(idx-1)) (idx-1)
        prop_all_proper_bits' (KLeaf kb) prefix idx = 
          bucketAll (\x -> x `nxor` prefix < 2 ^ idx) kb

-- Checks for duplicates
prop_no_duplicates (KNode l r) = prop_no_duplicates l && prop_no_duplicates r
prop_no_duplicates (KLeaf (KBucket s)) = nub sl == sl
  where sl = F.toList s
