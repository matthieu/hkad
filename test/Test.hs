import KTable

import Test.QuickCheck.Batch
import Test.QuickCheck

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.List(nub)

options = TestOptions
  { no_of_tests         = 50
  , length_of_tests     = 0
  , debug_tests         = False }

main =
  runTests "ktree" options
    [ run prop_max_k_bucket
    , run prop_all_proper_bits
    , run prop_no_duplicates ]

--
-- Quickcheck definitions and property checkers for KTree
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
