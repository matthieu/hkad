import KTable

import Test.QuickCheck.Parallel
-- import Test.QuickCheck.Batch
import Test.QuickCheck

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.List(nub)
import Data.Bits
import System.Random(randomRIO)
import Debug.Trace

-- options = TestOptions
--   { no_of_tests         = 200
--   , length_of_tests     = 0
--   , debug_tests         = False }
-- 
-- main =
--   runTests "ktree" options
--     [ run prop_max_k_bucket
--     , run prop_all_proper_bits
--     , run prop_no_duplicates
--     , run prop_kclosest_k ]


main = do
  t <- randomRIO (0,100000)
  pRun t 100 [ ("buckets max", pDet prop_max_k_bucket)
            , ("buckets bits position", pDet prop_all_proper_bits)
            , ("no duplicate", pDet prop_no_duplicates)
            , ("k retrieval", pDet prop_kclosest_k)
            , ("kclosest close", pDet prop_kclosest_close) ]

--
-- Quickcheck definitions and property checkers for KTree
--

instance Arbitrary KTree where
  arbitrary = sized (\x -> treeBuild (x * 20))

treeBuild 0 = return $ kbleaf S.empty
treeBuild n = do t <- treeBuild (n-1)
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

-- Checks that kclosest finds a least k nodes
prop_kclosest_k:: KTree -> Integer -> Bool
prop_kclosest_k kt nid = let kc = kclosest kt nid
                         in length kc >= kdepth || length kc == ktreeSize kt

-- Checks kclosest are actually close to pivot
prop_kclosest_close kt nid = all (\x -> x `xor` nid > kmaxDist || x `elem` kc) $ allNodes kt
  where kmaxDist = maximum $ map (xor nid) kc
        allNodes (KNode l r) = allNodes l ++ allNodes r
        allNodes (KLeaf kb) = bucketToList kb
        kc = kclosest kt nid
