{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- | Module    :  Math.Topology.CubeCmplx.CornerReduce
-- Copyright   :  2014 Michael Misamore 
-- License     :  BSD-style
-- Maintainer  :  m.misamore@gmail.com 
-- Stability   :  experimental 
-- Portability :  portable
--
-- Algorithms for simplifying finite directed cubical complexes up to directed
-- homotopy equivalence by removing corner vertices

module Math.Topology.CubeCmplx.CornerReduce (

   -- * Finding corners
   cmplxCornersNaive, cmplxSpanIntCorners, cmplxCorners, cmplxCornersInt,

   -- * Serial corner removal
   cmplxReduce', cmplxReduce, 
  
   -- * Parallelized corner removal
   cmplxReducePar', cmplxReducePar

) where

import Data.List (transpose, groupBy, sortBy, partition)
import qualified Data.Vector.Unboxed as V 
   ((//), sum, toList, all, fromList, Vector, zipWith, length, map, update, 
   (!), replicate, elemIndex, elemIndices, (++), zip, enumFromN, drop, take, 
   singleton, Unbox, accumulate)
import qualified Data.HashSet as S 
   (HashSet, fromList, filter, toList, union, empty, unions, 
    difference, map, size, singleton, null, foldr, delete, member)
import qualified Data.HashMap.Strict as M 
   (HashMap, empty, null, insertWith, fromListWith, filter, 
    fromList, lookup, toList)
import Control.Parallel.Strategies 
   (rdeepseq, parBuffer, withStrategy, parList, dot, evalTuple3, r0)
import Control.Arrow ((***))
import Math.Topology.CubeCmplx.DirCubeCmplx

-- | Naive algorithm for finding corner vertices of a cubical complex. Works
--   well whenever the complex is small
cmplxCornersNaive :: CubeCmplx -> S.HashSet (Vertex, CubeCell)
cmplxCornersNaive cx = S.fromList . map (\(v,(c,_)) -> (v,c)) . M.toList .
                       M.filter ((==1) . snd) $ 
                       M.fromListWith (\(c,n1) (_,n2) -> (c,n1+n2)) vcs 
   where cs  = S.toList $ cells cx
         vcs = concatMap (\c -> zip (verts c) (zip (repeat c) (repeat 1))) cs 

-- | Given a non-empty complex cx where all vertices have nonzero coordinates,
--   provide a list of vertex spans that can be used to detect every corner 
--   vertex of cx
cmplxCovSpans :: CubeCmplx -> [VertSpan]
cmplxCovSpans cx
   | cmplxNull cx = []
   | otherwise = vsCov3 $ cmplxCovHullUnsafe cx
   where vsCov3 vs = map (\e -> vsUnsafe (vertexUnsafe $ map fst e) 
                                         (vertexUnsafe $ map snd e)) $ rProds vs
         ranges vs = zip (vsFstList vs) (vsSndList vs)
         f (n,m)   = [(i,i+3) | i <- [n..m], i+3 <= m] 
         rProds vs = lazyProd $ map f (ranges vs)

-- | Given a cubical complex and a vertex span to which it belongs, determine
--   the set of corner vertices that belong to the interior of the span using
--   the naive algorithm
cmplxSpanIntCorners :: CubeCmplx -> VertSpan -> S.HashSet (Vertex, CubeCell)
cmplxSpanIntCorners cx vs = S.filter (\(v,_) -> not $ (vertToCell v) `inBdry` vs) 
                                     (cmplxCornersNaive cx)

-- | Memory-efficient parallelized algorithm for determining corner vertices of 
--   any finite directed cubical complex whose vertices all have nonzero 
--   coordinates
cmplxCorners :: CubeCmplx -> S.HashSet (Vertex, CubeCell)
cmplxCorners cx = S.unions . withStrategy (parBuffer 100 rdeepseq) . 
                  map (uncurry cmplxSpanIntCorners) $ 
                  cmplxFilterSpans cx (cmplxCovSpans cx)

-- | Given a cubical complex where all vertices have nonzero coordinates, 
--   determine the set of corner vertices that belong to the interior of the 
--   given span using the parallel algorithm
cmplxCornersInt :: CubeCmplx -> VertSpan -> S.HashSet (Vertex, CubeCell)
cmplxCornersInt cx vs 
   = S.filter (\(v,_) -> (v `vInSpan` vs) &&
                         (not $ (vertToCell v) `inBdry` vs)) (cmplxCorners cx)

-- | Given a nonempty complex cx where all vertices have nonzero coordinates,
--   get minimal vertex span properly containing cx that extends at least 3 
--   units along every coordinate. In particular, the result will have the
--   same dimension as the ambient space.
cmplxCovHullUnsafe :: CubeCmplx -> VertSpan
cmplxCovHullUnsafe cx = vsUnsafe f' s'
   where (f,s) = (vsFst $ cmplxHullUnsafe cx, vsSnd $ cmplxHullUnsafe cx)
         d     = s `vSubUnsafe` f
         f'    = f `vSub` (vertexUnsafe $ replicate (vDim f) 1)  
         s'    = vertexPtWise (\s1 d1 -> if d1 < 2 then s1+(2-d1) else s1+1) s d

-- | Given a dimension n, get set of all subcomplexes Lx supported away
--   from chosen vertices x in a generic n-cell, together with potential
--   new corner vertex for each potential new top-cell (memoized)
genLx :: Int -> M.HashMap Vertex [(Vertex, CubeCell)]
genLx n = gensLx !! n
gensLx = map genLx' [0..] where 
genLx' n = M.fromList . zip vs . zipWith zip (map xs vs) . 
           map (uncurry (zipWith cellVertsUnsafe)) $ zip (map ls vs) (map us vs)
      where z    = V.replicate n (0 :: T)
            o    = V.replicate n (1 :: T)
            vs   = map minVert $ nCubeVerts n         
            nx   = V.toList . V.zip (V.enumFromN 0 n) . V.map (\e -> 1-e) . coords
            ls   = map (vertexVectorUnsafe . V.update z . V.singleton) . nx 
            us   = map (vertexVectorUnsafe . V.update o . V.singleton) . nx 
            xs   = \v -> map (vertexVectorUnsafe . V.update (coords v) 
                              . V.singleton) (nx v)

-- | Given a (nongeneric) cell and a (nongeneric) vertex x belonging to it, 
--   return the set of subcells defining Lx and their unique corner vertices
cellLx :: CubeCell -> Vertex -> S.HashSet (Vertex, CubeCell)
cellLx c x = let cs = case M.lookup gx $ genLx (cellDim c) of
                         Nothing -> S.empty
                         Just cs -> S.fromList cs in
                S.map (\(v1,c1) -> (minVert $ genToNonGen c (vertToCell v1),
                                    genToNonGen c c1)) cs
   where gx = minVert $ nonGenToGen c (vertToCell x) 

-- | Given a cubical complex all of whose vertices have nonzero coordinates
--   and a corner vertex/cell pair to delete, determine any new corner 
--   vertex/cell pairs, any new top-cells that would be created, and any 
--   potential new top-cells that already were in the complex
cmplxCornerUpdate :: CubeCmplx -> (Vertex, CubeCell)   -- corner to process
                     -> (S.HashSet (Vertex, CubeCell), -- new corners
                         S.HashSet CubeCell,           -- new top-cells
                         S.HashSet CubeCell)           -- new top-cells omitted
cmplxCornerUpdate cx (x,c) = (cvs, newTops, notTops) 
   where delNhd   = cmplxDelCell (cellNhd cx c) c
         delNhdcs = S.toList $ cells delNhd
         (notTops,newTops) = S.fromList *** S.fromList $ 
                             partition (\c -> any (==True) $ 
                                        map (isSubCell c) delNhdcs) $
                             S.toList . S.map snd $ cellLx c x
         cvs      = S.filter (\(v1,_) -> (vertToCell v1) `isSubCell` c) . 
                    cmplxCornersNaive $ cmplxAddCells delNhd newTops 

-- | Given a set of (vertex,cell) pairs, return a maximal subset such that 
--   every cell occurs in at most one pair
uniqueCorners :: S.HashSet (Vertex, CubeCell) -> S.HashSet (Vertex, CubeCell)
uniqueCorners vcs = S.fromList . map (\(a,b) -> (b,a)) . M.toList $ 
                    S.foldr step M.empty vcs
   where step (v,c) m = M.insertWith (\v1 v2 -> v1) c v m

-- | Given a complex and a set of corner vertices, remove them one at a time
--   to get a new cubical complex with new corner vertices. Optionally exclude 
--   some corner vertices from consideration
cmplxCornersDelSerial :: 
   [VertSpan]  -- vertices to exclude from corner verts
   -> (CubeCmplx, S.HashSet (Vertex, CubeCell)) -- input cmplx/corner verts
   -> (CubeCmplx, S.HashSet (Vertex, CubeCell)) -- output cmplx/new corners
cmplxCornersDelSerial vs (cx,xs) = S.foldr step (cx,xs') corners 
   where vFilt               = S.filter (\(v,_) -> not . any (==True) $ 
                                                   map (vInSpan v) vs) 
         xs'                 = vFilt xs 
         corners             = uniqueCorners xs' --choose one corner per cell
         step (v,c) (cx',cs) = (if cellDim c == 0 then cx'
                                else cmplxAddCells (cmplxDelCell cx' c) $
                                                   (newTops cx' (v,c)),
                                vFilt $ if cellDim c == 0 then cs
                                        else S.union (delVerts cs c)
                                                     (newCorn cx' (v,c)))
         update cx' (v,c)  = cmplxCornerUpdate cx' (v,c)  
         newTops cx' (v,c) = (\(_,s,_) -> s) $ update cx' (v,c)
         newCorn cx' (v,c) = (\(f,_,_) -> f) $ update cx' (v,c)
         delVerts cs c     = S.difference cs . S.fromList $ 
                             zip (verts c) (repeat c)

-- | Given a complex, a set of corner vertices, and a set of corner vertices
--   to exclude from consideration, iteratively reduce the complex one corner
--   vertex at a time until there are no known non-excluded corner vertices 
--   remaining 
cmplxReduceSerial :: CubeCmplx -> S.HashSet (Vertex, CubeCell) -> [VertSpan] 
                     -> CubeCmplx
cmplxReduceSerial cx xs vs
   | xs == xs' = cx'
   | otherwise = cmplxReduceSerial cx' xs' vs
   where (cx',xs') = cmplxCornersDelSerial vs (cx,xs) 

-- | Given a cubical complex all of whose vertices have nonzero coordiantes
--   and a list of corner vertex/cell pairs to delete (one corner per cell), 
--   determine if these updates are parallelizable and describe the update 
cmplxCornerUpdates :: 
   CubeCmplx -> [(Vertex, CubeCell)] -- list of corners to process
             -> Maybe (S.HashSet (Vertex, CubeCell), -- new corners
                       S.HashSet CubeCell,           -- new top-cells
                       S.HashSet CubeCell)           -- new top-cells omitted
cmplxCornerUpdates cx xss = if paraPoss
                            then Just (S.unions xs, S.unions cs, S.unions os)
                            else Nothing 
   where updates    = map (cmplxCornerUpdate cx) xss
         (xs,cs,os) = unzip3 updates
         paraPoss   = M.null . M.filter (>= 2) . M.fromListWith (+) $ 
                      zip (concatMap (verts . snd) xss) (repeat 1)

-- | Given a complex all of whose vertices have nonzero coordinates, and a set 
--   of corner vertices, try to remove a subset of them simultaneously to get 
--   a new cubical complex with new corner vertices. Optionally exclude some 
--   vertex spans from consideration, and remove corners serially if necessary
cmplxCornersDelPar :: Int -- ^ Remove every nth vertex; 10 recommended
   -> [VertSpan]          -- ^ Vertex spans to exclude from corners
   -> (CubeCmplx, S.HashSet (Vertex, CubeCell)) -- ^ Input cmplx/corners
   -> (CubeCmplx, S.HashSet (Vertex, CubeCell)) -- ^ Output cmplx/new corners
cmplxCornersDelPar frac vs (cx,xs) 
   = case cmplxCornerUpdates cx (S.toList corners) of
        Just (ncs,nts,_) -> (cmplxAddCells (cmplxDelCells cx 
                                            (S.filter ((/=0).cellDim) . 
                                             S.map snd $ corners)) nts,
                             vFilt $ S.filter (\(v,c) -> (cellDim c == 0) ||
                                               not (v `S.member` delVerts)) xs' 
                                     `S.union` ncs)
        Nothing          -> cmplxCornersDelSerial vs (cx,xs)
   where vFilt    = S.filter (\(v,_) -> not . any (==True) $ 
                                        map (vInSpan v) vs) 
         xs'      = vFilt xs 
         corners  = S.fromList . cStrat . S.toList $ uniqueCorners xs' 
         delVerts = S.fromList . concatMap (verts.snd) $ S.toList corners
         cStrat   = map snd . filter ((==1).fst) . zip (cycle [1..frac])

-- | Given a complex all of whose vertices have nonzero coordinates, a set of 
--   corner vertices, and a list of vertex spans to exclude from consideration, 
--   iteratively reduce the complex until there are no known non-excluded 
--   corner vertices remaining. Return the intermediate complexes/corners 
--   in a list
cmplxReduce' :: Int -- ^ Try to remove every nth corner vertex on each round
                -> CubeCmplx                    -- ^ Complex to reduce 
                -> S.HashSet (Vertex, CubeCell) -- ^ Corner verts/cells
                -> [VertSpan]                   -- ^ Excluded vertex spans
                -> [(CubeCmplx, S.HashSet (Vertex, CubeCell), [VertSpan])]
cmplxReduce' frac cx xs vs = iterate go (cx,xs,vs) where 
   go (cx',xs',vs') = (ncx,nxs,vs) 
      where (ncx,nxs) = cmplxCornersDelPar frac vs (cx',xs')

-- | Given a complex all of whose vertices have nonzero coordinates, a set of 
--   corner vertices, and a list of vertex spans to exclude from consideration,
--   iteratively reduce the complex until there are no known non-excluded 
--   corner vertices remaining 
cmplxReduce :: Int -- ^ Try to remove every nth corner vertex on each round
               -> CubeCmplx                    -- ^ Complex to reduce 
               -> S.HashSet (Vertex, CubeCell) -- ^ Corner verts/cells
               -> [VertSpan]                   -- ^ Excluded vertex spans
               -> CubeCmplx
cmplxReduce frac cx xs vs = (\(ncx,_,_) -> ncx) . fst . head . 
                            dropWhile (\((_,xs,_),(_,ys,_)) -> xs /= ys) $
                            zip rcx (tail rcx)
   where rcx = cmplxReduce' frac cx xs vs

-- | Given a vertex span, determine a disjoint union of vertex spans that cover
--   a substantial portion of this span
disjointCov :: VertSpan -> [VertSpan]
disjointCov vs = map (buildvs . transpose) . lazyProd . 
                 map f $ zip (vsFstList vs) (vsSndList vs) 
   where f (i,j) | j <= i+2  = [[i,j]]
                 | otherwise = [[i,j `div` 2], [(j `div` 2)+1, j]]
         buildvs [t1,t2] = vsCoordsUnsafe t1 t2 

-- | Given complex all of whose vertices have nonzero coordinates and a vertex
--   span, filter for those cells belonging to span, determine the corner 
--   vertex/cell pairs interior to the span, and union with all cells
--   adjacent to the boundary of the span. Represents a parallelizable
--   "subproblem" for reduction via corner vertices
rSubProb :: CubeCmplx -> VertSpan 
            -> (CubeCmplx, S.HashSet (Vertex, CubeCell), [VertSpan])
rSubProb cx vs = (fatCmplx, corners, vsBdry vs)
   where subCmplx = cmplxFilterSpan cx vs
         corners  = cmplxCornersInt subCmplx vs
         c        = head $ spanTopCells vs 
         d        = (maxVert c) `vSub` (minVert c) 
         fatCmplx = cmplxFilterSpan cx $ vsUnsafe ((vsFst vs) `vSub` d) 
                                                  ((vsSnd vs) `vAdd` d)

-- | Given a complex whose vertices have nonzero coordinates, determine a
--   minimal vertex span containing it, find a disjoint union of vertex spans
--   that almost cover this span, and use these to determine a list of
--   subcomplexes that can be reduced in parallel subject to boundary conditions
rSubProbs :: CubeCmplx -> [(CubeCmplx, S.HashSet (Vertex, CubeCell), [VertSpan])]
rSubProbs cx = map (rSubProb cx) . disjointCov . vsFatten . 
               cmplxHullUnsafe $ cx

-- | Given a complex whose vertices have nonzero coordinates, reduce it in
--   parallel, optionally excluding some spans from the set of corner vertices. 
--   Return the intermediate complexes in a list
cmplxReducePar' :: CubeCmplx -> [VertSpan] -> [CubeCmplx]
cmplxReducePar' cx vs = iterate go cx where 
   go cx' = serStep . parStep $ cx'
      where f (cx',xs',vs') = cmplxReduce 10 cx' xs' (vs ++ vs')
            parStep         = cmplxUnions . withStrategy (parList rdeepseq) . 
                              map f . rSubProbs 
            serStep cx      = (\(x,_,_) -> x) . head . drop 1 $
                              cmplxReduce' 10 cx (cmplxCorners cx) vs

-- | Given a complex whose vertices have nonzero coordinates, reduce it in
--   parallel, optionally excluding some spans from the set of corner vertices. 
cmplxReducePar :: CubeCmplx -> [VertSpan] -> CubeCmplx
cmplxReducePar cx vs = fst . head . dropWhile (\(c,d) -> c /= d) $ 
                       zip rcx (tail rcx) 
   where rcx = cmplxReducePar' cx vs 

