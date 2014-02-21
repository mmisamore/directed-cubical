-- Test suite

import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Math.Topology.CubeCmplx.DPTrace
import qualified Data.HashSet as S (size)
import Data.Maybe (fromJust)
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen, Gen)
import System.Random (mkStdGen)

-- Functions for testing --
rList :: Gen a -> Int -> [a]
rList g n = (unGen (listOf g)) (mkStdGen n) n

-- Property tests --
prop_spanTopCells vs    = not $ null (spanTopCells vs) 
prop_vertSpan vs        = vsFst vs `vLT` vsSnd vs

prop_vsBdry vs          = all (==True) . map (prop_vertSpan) $ vsBdry vs
prop_cmplxHullUnsafe cx = if cmplxNull cx then True
                          else prop_vertSpan $ cmplxHullUnsafe cx 

-- Fuzz tests --
fuzz_cellNhd n          = map (uncurry cellNhd) $ 
                          zip (rList (arbitrary :: Gen CubeCmplx) n)
                              (rList (arbitrary :: Gen CubeCell) n)

fuzz_cmplxCornersNaive n   = map cmplxCornersNaive $
                                rList (arbitrary :: Gen CubeCmplx) n
fuzz_cmplxCorners n        = map cmplxCorners $ rList (arbitrary :: Gen CubeCmplx) n
fuzz_cmplxSpanIntCorners n = map (uncurry cmplxSpanIntCorners) $
                                 zip (rList (arbitrary :: Gen CubeCmplx) n)
                                     (rList (arbitrary :: Gen VertSpan) n)
fuzz_cmplxCornersInt n   = map (uncurry cmplxCornersInt) $
                              zip (rList (arbitrary :: Gen CubeCmplx) n)
                                  (rList (arbitrary :: Gen VertSpan) n)
fuzz_vsFatten n          = map vsFatten $ rList (arbitrary :: Gen VertSpan) n
fuzz_vsCornerPairs n     = map vsCornerPairs $ rList (arbitrary :: Gen VertSpan) n
fuzz_cmplxHullUnsafe n   = map cmplxHullUnsafe $ filter ((==False).cmplxNull) $ 
                              rList (arbitrary :: Gen CubeCmplx) n
--fuzz_rSubProb n          = map (uncurry rSubProb) xs 
--                           where xs = zip (rList (arbitrary :: Gen CubeCmplx) n)
--                                          (rList (arbitrary :: Gen VertSpan) n)
--fuzz_rSubProbs n         = map rSubProbs $ filter ((==False).cmplxNull) $
--                           rList (arbitrary :: Gen CubeCmplx) n
--fuzz_disjointCov n       = map disjointCov $ rList (arbitrary :: Gen VertSpan) n
fuzz_cmplxReduce' n   = map (flip cmplxReduce' []) $ 
                              rList (arbitrary :: Gen CubeCmplx) n
fuzz_cmplxReduce n    = map (flip cmplxReduce []) $ 
                              rList (arbitrary :: Gen CubeCmplx) n
fuzz_cmplxDelVsInt n  = map (uncurry cmplxDelVsInt) $
                           zip (rList (arbitrary :: Gen CubeCmplx) n)
                               (rList (arbitrary :: Gen VertSpan) n)

-- Example tests --
eg_sqPairBack         = (S.size $ cells (uncurry cmplxReduce $ sqPairBack)) == 15
eg_sqPairFwd          = (S.size $ cells (uncurry cmplxReduce $ sqPairFwd))  == 14
eg_swissFlag          = (S.size $ cells (uncurry cmplxReduce $ swissFlag))  == 16
eg_torus3d            = (S.size $ cells (uncurry cmplxReduce $ torus3d))    == 8
eg_genusTwo3d         = (S.size $ cells (uncurry cmplxReduce $ genusTwo3d)) == 11

-- Represent the classic swiss flag complex as a process trace problem
eg_swissFlag_pt       = (fst $ swissFlag) ==
                        ptsCmplx [fromJust $ pTrace 1 [(P,1),(P,2),(V,2),(V,1)],
                                  fromJust $ pTrace 2 [(P,2),(P,1),(V,1),(V,2)]]

