{-# LANGUAGE TypeSynonymInstances #-} 

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


main =
  pRun 4 500  [("buckets max", pDet prop_max_k_bucket)
            , ("buckets bits position", pDet prop_all_proper_bits)
            , ("no duplicate", pDet prop_no_duplicates)
            , ("k retrieval", pDet prop_kclosest_k)
            , ("kclosest close", pDet prop_kclosest_close)
            , ("kbucketsRange length", pDet prop_kbucketsRange_length)]

--
-- Quickcheck definitions and property checkers for KTree
--

data TestNode = TN { testNodeId :: Integer }
  deriving (Show, Eq, Ord)

instance Node TestNode where
  nodeId = testNodeId

instance (Eq a, Node a, Arbitrary a) => Arbitrary (KTree a) where
  arbitrary = sized (\x -> treeBuild (x * 20))

treeBuild 0 = return $ kbleaf S.empty
treeBuild n = do t <- treeBuild (n-1)
                 v <- arbitrary
                 return $ kinsert 0 t v

instance Arbitrary TestNode where
  arbitrary = do
    nid  <- choose (0, 2^binaries-1) :: Gen Integer
    return $ TN nid

-- Checks that all buckets have k or less elements
prop_max_k_bucket :: KTree TestNode -> Bool
prop_max_k_bucket = F.foldr ((&&) . (<= kdepth) . S.length) True

-- Checks that all bucket element have the right bit sequence with its tree position
prop_all_proper_bits :: KTree TestNode -> Bool
prop_all_proper_bits (KLeaf b) = True
prop_all_proper_bits kn@(KNode _ _) = prop_all_proper_bits' kn 0 binaries
  where prop_all_proper_bits' (KNode l r) prefix idx = 
          prop_all_proper_bits' l prefix (idx-1) 
                   && prop_all_proper_bits' r (prefix+2^(idx-1)) (idx-1)
        prop_all_proper_bits' (KLeaf kb) prefix idx = 
          F.all (\x -> (nodeId x) `nxor` prefix < 2 ^ idx) kb

-- Checks for duplicates
prop_no_duplicates :: KTree TestNode -> Bool
prop_no_duplicates = F.foldr (\kb b -> b && nub (F.toList kb) == (F.toList kb)) True

-- Checks that kclosest finds a least k nodes
prop_kclosest_k :: KTree TestNode -> TestNode -> Bool
prop_kclosest_k kt peer = let kc = kclosest kt (nodeId peer)
                          in length kc >= kdepth || length kc == ktreeSize kt

-- Checks kclosest are actually close to pivot
prop_kclosest_close :: KTree TestNode -> TestNode -> Bool
prop_kclosest_close kt peer = all (\x -> (nodeId x) `xor` nid > kmaxDist || x `elem` kc) $ ktreeList kt
  where kmaxDist = maximum $ map (xor nid . nodeId) kc
        kc  = kclosest kt nid
        nid = nodeId peer

not_closest kt peer = filter (\x -> (nodeId x) `xor` nid <= kmaxDist && x `notElem` kc) $ ktreeList kt
  where kmaxDist = maximum $ map (xor nid . nodeId) kc
        kc  = kclosest kt nid
        nid = nodeId peer

prop_kbucketsRange_length :: KTree TestNode -> Bool
prop_kbucketsRange_length kt = F.foldr (const (+1)) 0 kt == length (kbucketsRange kt)
