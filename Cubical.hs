{-- Description: Types and functions for cubical complexes
    Author:      Mike Misamore
    Updated:     10 October 2013
--}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

import Data.Int      (Int8)
import Data.Maybe    (fromJust)
import Data.List     (transpose, groupBy, sortBy, partition)
import Data.Ord      (comparing)
import Data.Function (on)
import Control.Monad (liftM, guard)
import Data.Hashable (Hashable, hashWithSalt, hash)
import qualified Data.HashSet as S 
   (HashSet, fromList, filter, toList, union, empty, unions, 
    difference, map, size, singleton, null, foldr, delete)
import qualified Data.HashMap.Strict as M 
   (HashMap, empty, insertWith, fromListWith, filter, fromList, lookup, toList)
import qualified Data.Vector.Unboxed as V 
   ((//), sum, toList, all, fromList, Vector, zipWith, length, map, update, 
   (!), empty, replicate, elemIndex, elemIndices, (++), cons, zip,
   enumFromN, unfoldr, drop, take, singleton, Unbox, accumulate)
import Data.Bits ((.&.), (.|.), xor, shiftR, shiftL)
import Data.Word (Word16)
import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies (rdeepseq, parBuffer, withStrategy)
import Control.Arrow ((***))
-- Testing libraries --
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, choose, vectorOf, resize) 

-- Utilities --

-- | Space-efficient cartesian product of list of finite domains
lazyProd :: [[a]] -> [[a]]
lazyProd []  = [[]]
lazyProd [x] = map (:[]) x
lazyProd (x1:x2:xs) = concat . concat $
                      [[[y1:y2:yn | y1<-x1] | y2<-x2] | yn <- (lazyProd xs)]

-- | Given a Word16, unpack it to a vector of bits in msb..lsb order 
unpackWord16 :: Word16 -> V.Vector Int 
unpackWord16 w = V.map fromEnum . fst $ foldr step (V.empty, w) [15,14..0]
   where step _ (v,w) = ((w .&. 1) `V.cons` v, w `shiftR` 1)

-- | Given a vector of 16 bits in msb..lsb order, pack it into a Word16 
packWord16Unsafe :: V.Vector Int -> Word16
packWord16Unsafe v = V.sum $ V.zipWith (*) (V.map toEnum v) ps 
ps = V.unfoldr (\n -> if n == 0 then Nothing 
                else Just (n, n `shiftR` 1)) (1 `shiftL` 15 :: Word16)

-- | Given a vector of n bits, a bitmask with k bits for n ambient coordinates,
--   and a vector f of k bits to add, fill in the vector along the bitmask with 
--   the fillers
bitFill :: V.Vector T -> V.Vector T -> V.Vector T -> V.Vector T
bitFill v m f = V.accumulate (+) v $ V.zip maskIndices f
   where maskIndices = V.elemIndices 1 m


-- Vertices --

-- | A generic notation for coordinate values
type T = Int8

-- | Random vectors
instance (Arbitrary a, V.Unbox a, Num a, Ord a) => Arbitrary (V.Vector a)
   where arbitrary = do l  <- choose (1,7) 
                        ts <- vectorOf l (arbitrary `suchThat` (>=0))
                        return $ V.fromList ts 

-- | A vertex with lexicographic ordering
data Vertex = Vertex { coords :: V.Vector T, _hash :: Int } deriving (Eq, Ord)
instance Show Vertex where show v = show (V.toList $ coords v)
instance Arbitrary Vertex where arbitrary = liftM vertexVectorUnsafe $ arbitrary
instance Hashable Vertex where hashWithSalt s v = s + (_hash v)
instance NFData Vertex where rnf v = (rnf $ coords v) `seq` 
                                     (rnf $ _hash v)  `seq` () 

-- | Safe constructor for vertices 
vertex :: [T] -> Maybe Vertex
vertex ts | null ts      = Nothing 
          | any (< 0) ts = Nothing
          | otherwise    = Just . vertexUnsafe $ ts 

-- | Unsafe constructor for vertices
vertexUnsafe :: [T] -> Vertex
vertexUnsafe ts = Vertex { coords = V.fromList ts, _hash = hash ts }

-- | Unsafe constructor for vertices from vectors
vertexVectorUnsafe :: V.Vector T -> Vertex
vertexVectorUnsafe v = Vertex { coords = v, _hash = hash $ V.toList v } 

-- | Fetch coordinates for a vertex
vertexToList :: Vertex -> [T]
vertexToList = V.toList . coords

--- | Combine two vertices coordinate-wise with a given operation,
--    with floor of 0 on each coordinate
vertexPtWise :: (T -> T -> T) -> Vertex -> Vertex -> Vertex
vertexPtWise f v1 v2 = vertexVectorUnsafe $
                       V.zipWith (\x y -> if (f x y) < 0 then 0 else f x y)
                          (coords v1) (coords v2)

-- | Add two vertices coordinate-wise
vAdd :: Vertex -> Vertex -> Vertex
vAdd = vertexPtWise (+)

-- | Subtract two vertices coordinate-wise
vSub :: Vertex -> Vertex -> Vertex
vSub = vertexPtWise (-)

-- | Subtract two vertices coordinate-wise without bounds checking
vSubUnsafe :: Vertex -> Vertex -> Vertex
vSubUnsafe v1 v2 = vertexVectorUnsafe $ V.zipWith (-) (coords v1) (coords v2)

-- | Coordinate-wise minimum
vMin :: Vertex -> Vertex -> Vertex
vMin = vertexPtWise (min)

-- | Coordinate-wise maximum
vMax :: Vertex -> Vertex -> Vertex
vMax = vertexPtWise (max)

-- | Test whether vertex is less than another in cubical partial ordering
vLT :: Vertex -> Vertex -> Bool
vLT v1 v2 = V.all (==True) $ V.zipWith (<=) (coords v1) (coords v2) 

-- | Test whether vertex is greater than another in cubical partial ordering
vGT :: Vertex -> Vertex -> Bool
vGT = flip vLT

-- | Fetch ambient dimension of a vertex
vDim :: Vertex -> Int
vDim = V.length . coords


-- Vertex Spans defining sets of cubical cells --

-- | A cubical vertex span
data VertSpan = VertSpan { vsFst :: !Vertex, vsSnd :: !Vertex } 
   deriving (Show, Eq, Ord)
instance NFData VertSpan
instance Arbitrary VertSpan
   where arbitrary = do v1 <- arbitrary
                        v2 <- (resize 6 arbitrary) 
                              `suchThat` ((== vDim v1) . vDim)
                        return $ vsUnsafe v1 (v1 `vAdd` v2)

-- | Safe constructor for vertex spans. Sanity checks for matching ambient
--   coordinate systems
vertSpan :: Vertex -> Vertex -> Maybe VertSpan
vertSpan v1 v2 
   | (v1 `vLT` v2) && (vDim v1 == vDim v2) = Just $ VertSpan v1 v2
   | otherwise = Nothing

-- | Unsafe constructor for vertex spans
vsUnsafe :: Vertex -> Vertex -> VertSpan
vsUnsafe = VertSpan

-- | Get coordinates for lower vertex in coordinate span
vsFstList :: VertSpan -> [T]
vsFstList = vertexToList . vsFst 

-- | Get coordinates for upper vertex in coordinate span
vsSndList :: VertSpan -> [T]
vsSndList = vertexToList . vsSnd 

-- | Safe constructor for vertex spans from coordinates
vsCoords :: [T] -> [T] -> Maybe VertSpan
vsCoords t1 t2 = do v1 <- vertex t1; v2 <- vertex t2; vertSpan v1 v2

-- | Unsafe constructor for vertex spans from coordinates
vsCoordsUnsafe :: [T] -> [T] -> VertSpan
vsCoordsUnsafe t1 t2 = vsUnsafe (vertexUnsafe t1) (vertexUnsafe t2) 

-- | Given a vertex span, determine the corresponding cubical dimension
vsDim :: VertSpan -> Int
vsDim vs = V.sum $ V.zipWith (\x y -> if x /= y then 1 else 0)
                             (coords $ vsFst vs) (coords $ vsSnd vs)

-- | Test whether a vertex span is a cubical cell
vsIsCell :: VertSpan -> Bool
vsIsCell vs = V.all (flip elem [0,1]) . coords $
              (vsSnd vs) `vSubUnsafe` (vsFst vs)

-- | Given a vertex span, efficiently determine all pairs of (cell,vertex)
--   where the vertices are corner vertices of the span and the cells are
--   the unique top-cells containing them
vsCornerPairs :: VertSpan -> S.HashSet (CubeCell, Vertex)
vsCornerPairs vs
   | vsDim vs == 0 = S.singleton $ (cellUnsafe (vsFstList vs) (vsSndList vs),
                                    vertexUnsafe (vsFstList vs))
   | otherwise = S.fromList $ zip cells corners 
   where coordSpans = transpose [vsFstList vs, vsSndList vs]
         coordRans  = map (\cs -> enumFromTo (head cs) (last cs)) coordSpans
         coordRans' = map (\cs -> enumFromThenTo (last cs) (last cs-1) 
                                                 (head cs)) coordSpans
         possCoords = zipWith (\l1 l2 -> [l1, reverse l2]) 
                      (map (take 2) coordRans) (map (take 2) coordRans') 
         cells      = map (\[x,y] -> cellUnsafe x y) . map transpose $ 
                      lazyProd possCoords
         corners    = map vertexUnsafe . lazyProd $ 
                      map (\[x,y] -> [head x, last y]) possCoords

-- | Given a vertex span, efficiently determine its "corner" vertices
vsCornerVerts :: VertSpan -> S.HashSet Vertex
vsCornerVerts = S.map snd . vsCornerPairs 


-- Cubical Cells --

-- | Type for bit vectors to help generate random cells
newtype BitVector = BitVector { bitVect :: V.Vector T } deriving (Show)
instance Arbitrary BitVector where
   arbitrary = do l  <- choose (1,7)
                  bs <- vectorOf l (choose (0,1))
                  return . BitVector $ V.fromList bs
 
-- | A cubical cell 
data CubeCell = CubeCell { _minVert :: !Vertex, _maxVert :: !Vertex } deriving (Eq)
instance NFData CubeCell

-- | For storing cubical cells in unordered containers
instance Hashable CubeCell
   where hashWithSalt s c = hashWithSalt s (_minVert c, _maxVert c)

-- | Lexicographically ordered by endpoints 
instance Ord CubeCell where
   c1 <= c2 = (minVert c1, maxVert c1) <= (minVert c2, maxVert c2)

-- | Show endpoints
instance Show CubeCell
   where show c = "(" ++ show (cellDim c) ++ ","
                  ++ show (minVert c) ++ "," ++ show (maxVert c) ++ ")"

-- | Random cubical cells
instance Arbitrary CubeCell
   where arbitrary = do v1 <- arbitrary
                        v2 <- (liftM (vertexVectorUnsafe . bitVect) $ arbitrary) 
                              `suchThat` ((== vDim v1) . vDim)
                        return $ cellVertsUnsafe v1 (v1 `vAdd` v2)

-- | Get the minimum vertex for a cubical cell
minVert :: CubeCell -> Vertex
minVert c = _minVert c

-- | Get the maximum vertex for a cubical cell
maxVert :: CubeCell -> Vertex
maxVert c = _maxVert c
 
-- | Unsafe constructor for cubical cells from vertices
cellVertsUnsafe :: Vertex -> Vertex -> CubeCell
cellVertsUnsafe v1 v2 = CubeCell v1 v2

-- | Unsafe constructor for cubical cells from coordinates
cellUnsafe :: [T] -> [T] -> CubeCell
cellUnsafe t1 t2 = cellVertsUnsafe (vertexUnsafe t1) (vertexUnsafe t2) 

-- | Safe constructor for cubical cells from vertices
cellVerts :: Vertex -> Vertex -> Maybe CubeCell
cellVerts v1 v2 = do vs <- vertSpan v1 v2 
                     guard (vsIsCell vs)
                     return $ cellVertsUnsafe v1 v2 

-- | Safe constructor for cubical cells from coordinates
cell :: [T] -> [T] -> Maybe CubeCell
cell t1 t2 = do v1 <- vertex t1; v2 <- vertex t2; cellVerts v1 v2

-- | Get dimension of a cell
cellDim :: CubeCell -> Int
cellDim c = fromEnum . V.sum . coords $ maxVert c `vSubUnsafe` minVert c

-- | Given a coordinate span, list its top-dimensional cubical cells
spanTopCells :: VertSpan -> [CubeCell] 
spanTopCells = map pairUp . vertSpans
   where pairUp [a,b] = cellUnsafe a b
         -- determine the vertex spans of top-dimensional cubes
         vertSpans vs = map transpose . lazyProd .
                        map (pairs . uncurry enumFromTo) $
                        zip (vsFstList vs) (vsSndList vs)
         pairs []  = []
         pairs [x] = [[x,x]]
         pairs xs  = zipWith (\a b -> [a,b]) xs (tail xs)

-- | Treat a vertex as a cell
vertToCell :: Vertex -> CubeCell
vertToCell v = cellVertsUnsafe v v

-- | Test if a cubical cell belongs to a given vertex span
inSpan :: CubeCell -> VertSpan -> Bool
inSpan c vs = (vsFst vs `vLT` minVert c) && (maxVert c `vLT` vsSnd vs)

-- | Type for recording comparisons to extrema
data VertType = Min | Max | Neither deriving (Show,Eq)

-- | Test if a cubical cell is in the boundary of a cubical coordinate span.
--   See also vsBdry and spanBdryCells 
inBdry :: VertSpan -> CubeCell -> Bool
inBdry vs c = any (==True) $
              zipWith (\a b -> a == b && a /= Neither)
                      (vertBdryCmpts vs $ minVert c)
                      (vertBdryCmpts vs $ maxVert c)
   where vertBdryCmpts vs v = zipWith3 cmp (vsFstList vs) (vsSndList vs)
                              (vertexToList v)
         cmp min max i | i == min = Min
                       | i == max = Max
                       | otherwise = Neither

-- | Given a coordinate span, list all coordinate spans of its boundary
vsBdry :: VertSpan -> [VertSpan]
vsBdry vs = map (uncurry vsUnsafe) (fstSnd fst ++ fstSnd snd)
   where ranges     = V.zip (coords $ vsFst vs) (coords $ vsSnd vs)
         modVec f i = V.take i ranges 
                      V.++ (V.singleton . (\t -> (t,t)) . f $ ranges V.! i) 
                      V.++ V.drop (i+1) ranges
         fstSnd f   = zip (map (vertexVectorUnsafe . 
                           V.map fst . modVec f) [0..V.length ranges-1])
                          (map (vertexVectorUnsafe . 
                           V.map snd . modVec f) [0..V.length ranges-1])
 
-- | Given a coordinate span, provide a list of top-cells in each face
spanBdryCells :: VertSpan -> [[CubeCell]]
spanBdryCells = map spanTopCells . vsBdry

-- | List of all possible generic n-cubes, presented as cells (memoized)
nCubes :: [CubeCell]
nCubes = map gen [0..]
   where gen n = cellUnsafe (replicate n 0) (replicate n 1)

-- | Vertices of generic n-cube, as subcells (memoized)
nCubeVerts :: Int -> [CubeCell]
nCubeVerts n | n < 0     = []
             | otherwise = nCubesVerts !! n
nCubesVerts = map nCubeVerts' [0..]
   where nCubeVerts' 0 = map (vertToCell . vertexUnsafe) [[0]]
         nCubeVerts' n = map (vertToCell . vertexUnsafe) . lazyProd $ 
                         replicate n [0,1]

-- | Subcells of a generic n-cube (memoized)
nCubeCells :: Int -> [CubeCell]
nCubeCells n | n < 0     = []
             | otherwise = nCubesCells !! n
nCubesCells = map nCubeCells' [0..]
   where nCubeCells' n = [cellVertsUnsafe v1 v2 | 
                          v1 <- map minVert $ nCubeVerts n, 
                          v2 <- map minVert $ nCubeVerts n, v1 `vLT` v2]

-- | Proper subcells of a generic n-cube (mostly memoized)
nCubeProperCells :: Int -> [CubeCell]
nCubeProperCells n = filter ((/= n) . cellDim) . nCubeCells $ n

-- | List of cells in boundary of a generic n-cube (memoized)
nCubeBdry :: Int -> [CubeCell]
nCubeBdry n | n < 0     = []
            | otherwise = nCubesBdry !! n
nCubesBdry = map nCubeBdry' [0..]
   where nCubeBdry' n = concat . spanBdryCells $ vsCoordsUnsafe
                        (replicate n 0) (replicate n 1)

-- | List top-cells in k-skeleta of generic n-cube (memoized)
nCubeKSkels :: Int -> Int -> [CubeCell]
nCubeKSkels n k | n < 0 || k < 0 = []
                | k > n = [nCubes !! n]
                | otherwise = nCubesKSkels !! n !! k
nCubesKSkels = map nCubeKSkels' [0..]
   where nCubeKSkels' = groupBy ((==) `on` cellDim) . 
                        sortBy (comparing cellDim) . nCubeCells

-- | Given a (nongeneric) cell c and a generic cell g representing a subcell
--   of a generic cell of dimension dim c, return the translation of g into 
--   the nongeneric coordinates of c 
genToNonGen :: CubeCell -> CubeCell -> CubeCell
genToNonGen c g = cellVertsUnsafe l u 
   where bitMask = coords $ maxVert c `vSubUnsafe` minVert c
         minc    = coords $ minVert c
         l       = vertexVectorUnsafe $ bitFill minc bitMask (coords $ minVert g)
         u       = vertexVectorUnsafe $ bitFill minc bitMask (coords $ maxVert g)

-- | Given a subcell s of a (nongeneric) cell c, express s as a subcell of
--   a generic cell of the same dimension as c
nonGenToGen :: CubeCell -> CubeCell -> CubeCell
nonGenToGen c s = cellUnsafe (zipWith (V.!) (repeat $ locMin) indices)
                             (zipWith (V.!) (repeat $ locMax) indices)
   where locMin    = coords $ minVert s `vSubUnsafe` minVert c
         locMax    = coords $ maxVert s `vSubUnsafe` minVert c
         bitMask   = coords $ maxVert c `vSubUnsafe` minVert c
         indices   = V.toList . V.elemIndices 1 $ bitMask

-- | Given a (nongeneric) cubical cell, get specified set of cubical
--   substructure
lookupSubCells :: [[CubeCell]] -> CubeCell -> [CubeCell]
lookupSubCells l c = map (genToNonGen c) $ l !! cellDim c

-- | Given a (nongeneric) cubical cell, list its vertices
verts :: CubeCell -> [Vertex]
verts c = map minVert $ lookupSubCells nCubesVerts c

-- | Given a (nongeneric) cubical cell, get all cubical subcells
subCells :: CubeCell -> [CubeCell]
subCells = lookupSubCells nCubesCells

-- | Given a (nongeneric) cubical cell, get all proper cubical subcells
properSubCells :: CubeCell -> [CubeCell]
properSubCells = lookupSubCells (map nCubeProperCells [0..])

-- | Given a (nongeneric) cubical cell of dim n in ambient dim n, 
--   get its boundary
bdry :: CubeCell -> [CubeCell]
bdry = lookupSubCells nCubesBdry

-- | Given a (nongeneric) cubical cell, get top-cells of its k-skeleton
kSkel :: Int -> CubeCell -> [CubeCell]
kSkel k c | k < 0 = []
          | otherwise  = map (genToNonGen c) gs 
   where gs = nCubeKSkels (cellDim c) k

-- | Test if the former cubical cell is a subcell of the latter
isSubCell :: CubeCell -> CubeCell -> Bool
isSubCell s c = inSpan s $ vsUnsafe (minVert c) (maxVert c)

-- | Test if the former cubical cell is a proper subcell of the latter 
isPropSubCell :: CubeCell -> CubeCell -> Bool
isPropSubCell s c = (isSubCell s c) && (cellDim c /= cellDim s)

-- | Lazy list of maps from faces of generic n-cubes to their opposites
genOpFaces :: [M.HashMap CubeCell CubeCell] 
genOpFaces = map opFaces [0..]
   where differ v1 v2   = V.zipWith xor (V.zipWith (.&.) v1 v2) 
                                        (V.zipWith (.|.) v1 v2)
         invert v1 v2   = V.map (xor 1) $ differ v1 v2
         index  v1 v2   = fromJust $ V.elemIndex 1 $ invert v1 v2
         newVal v1 v2   = (index v1 v2, 1 - v1 V.! index v1 v2)
         newVerts v1 v2 = map (vertexUnsafe . V.toList . 
                               flip (V.//) [newVal v1 v2]) [v1, v2]
         opVerts c      = newVerts (coords $ minVert c) (coords $ maxVert c) 
         opFace c       = cellVertsUnsafe (head $ opVerts c) (last $ opVerts c) 
         opFaces n      = M.fromList . zip (nCubesBdry !! n) $ 
                          map (opFace) (nCubesBdry !! n) 

-- | Given a face f in some n-cube, get its opposite face (memoized)
opFaceUnsafe :: CubeCell -> CubeCell -> CubeCell
opFaceUnsafe c f = let g = fromJust $ M.lookup f' (genOpFaces !! (cellDim c))
                   in genToNonGen c g
   where f' = nonGenToGen c f


-- Cubical Complexes --

-- | A cubical complex consists of a set of top-cells
newtype CubeCmplx = CubeCmplx { cells :: S.HashSet CubeCell } deriving (Show,Eq)
instance NFData CubeCmplx where rnf cx = rnf (cells cx)

-- | A "random" cubical complex will be a vertex span with a random subset of
--   top-cells removed. Not ideal since every cell will have the same dimension
instance Arbitrary CubeCmplx
   where arbitrary = do vs <- arbitrary
                        let cx = vsCmplx vs
                        let cs = zip (cycle [1..100]) $ S.toList (cells cx)
                        return . CubeCmplx . S.fromList . map snd .
                           filter ((>=10) . fst) $ cs

-- | An empty complex 
cmplxEmpty :: CubeCmplx
cmplxEmpty = CubeCmplx { cells = S.empty }

-- | Detect if complex is empty
cmplxNull :: CubeCmplx -> Bool
cmplxNull cx = S.null $ cells cx

-- | Get the size of a cubical complex
cmplxSize :: CubeCmplx -> Int
cmplxSize cx = S.size $ cells cx

-- | Given a function producing a set of cubical cells from any cubical cell,
--   apply it to a cubical complex to yield a new complex
cmplxApply :: (CubeCell -> S.HashSet CubeCell) -> CubeCmplx -> CubeCmplx
cmplxApply f cx = CubeCmplx . S.unions . map f . S.toList $ cells cx

-- | Basic means of constructing cubical complexes via vertex spans
vsCmplx :: VertSpan -> CubeCmplx
vsCmplx vs = CubeCmplx { cells = S.fromList $ spanTopCells vs }

-- | Given a single cell to delete from a complex, delete it if present
cmplxDelCell :: CubeCmplx -> CubeCell -> CubeCmplx
cmplxDelCell cx c = CubeCmplx { cells = S.delete c (cells cx) }

-- | Given a list of cells to delete from a complex, delete them if present
cmplxDelCells :: CubeCmplx -> S.HashSet CubeCell -> CubeCmplx
cmplxDelCells cx cs = CubeCmplx { cells = S.difference (cells cx) cs }

-- | Given a set of cells to insert into a complex, insert them all
cmplxAddCells :: CubeCmplx -> S.HashSet CubeCell -> CubeCmplx
cmplxAddCells cx cs = CubeCmplx { cells = S.union cs (cells cx) }

-- | Filter the top-cells of a complex on some predicate
cmplxFilter :: (CubeCell -> Bool) -> CubeCmplx -> CubeCmplx
cmplxFilter f cx = CubeCmplx . S.filter f $ cells cx

-- | Given a nonempty complex, determine the minimal vertex span containing it.
--   The resulting span need not have the same dimension as the ambient space.
cmplxHullUnsafe :: CubeCmplx -> VertSpan
cmplxHullUnsafe cx = vsUnsafe minv maxv
   where (f,s) = unzip . map (\c -> (minVert c, maxVert c)) . S.toList $ cells cx
         minv  = foldr vMin (vertexUnsafe $ replicate (vDim $ head f) 
                            (maxBound :: T)) f
         maxv  = foldr vMax (vertexUnsafe $ replicate (vDim $ head f)
                            (minBound :: T)) s 
 
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

-- | Given a vertex span extending at least 3 units along every coordinate,
--   determine a covering of it by subspans generated by all possible 
--   consecutive sequences of length 4 in every coordinate
vsCov :: VertSpan -> [VertSpan]
vsCov vs = map (\e -> vsUnsafe (vertexUnsafe $ map fst e) 
                               (vertexUnsafe $ map snd e)) rProds
   where ranges  = zip (vsFstList vs) (vsSndList vs)
         f (n,m) = [(i,i+3) | i <- [n..m], i+3 <= m] 
         rProds  = lazyProd $ map f ranges 

-- | Given a complex cx where all vertices have nonzero coordinates, provide a 
--   list of vertex spans that can be used to detect every corner vertex of cx
cmplxCovSpans :: CubeCmplx -> [VertSpan]
cmplxCovSpans cx
   | cmplxNull cx = []
   | otherwise = vsCov $ cmplxCovHullUnsafe cx

-- | Given a complex cx and a vertex span vs, filter the complex down to the
--   subcomplex of all top-cells of cx contained in vs
cmplxFilterSpan :: CubeCmplx -> VertSpan -> CubeCmplx
cmplxFilterSpan cx vs = cmplxFilter (flip inSpan vs) cx 

-- | Given a complex and a list of vertex spans, determine the list of
--   subcomplexes of top-cells supported on these spans, paired up with the
--   spans so that the original boundaries are known
cmplxFilterSpans :: CubeCmplx -> [VertSpan] -> [(CubeCmplx, VertSpan)]
cmplxFilterSpans cx vss = withStrategy (parBuffer 100 rdeepseq) $
                          zip (map (cmplxFilterSpan cx) vss) vss

-- | Naive algorithm for finding corner vertices of a cubical complex. Works
--   well whenever the complex is small
cmplxCornersNaive :: CubeCmplx -> S.HashSet (Vertex, CubeCell)
cmplxCornersNaive cx = S.fromList . map (\(v,(c,_)) -> (v,c)) . M.toList .
                       M.filter ((==1) . snd) $ 
                       M.fromListWith (\(c,n1) (_,n2) -> (c,n1+n2)) vcs 
   where cs  = S.toList $ cells cx
         vcs = concatMap (\c -> zip (verts c) (zip (repeat c) (repeat 1))) cs 

-- | Given a cubical complex and a vertex span to which it belongs, determine
--   the set of corner vertices that belong to the interior of the span
cmplxSpanIntCorners :: CubeCmplx -> VertSpan -> S.HashSet (Vertex, CubeCell)
cmplxSpanIntCorners cx vs = S.filter (\(v,_) -> not $ inBdry vs (vertToCell v)) 
                                     (cmplxCornersNaive cx)

-- | Memory-efficient parallelized algorithm for determining corner vertices of any
--   finite directed cubical complex whose vertices all have nonzero coordinates
cmplxCorners :: CubeCmplx -> S.HashSet (Vertex, CubeCell)
cmplxCorners cx = S.unions . withStrategy (parBuffer 100 rdeepseq) . 
                  map (uncurry cmplxSpanIntCorners) $ 
                  cmplxFilterSpans cx (cmplxCovSpans cx)

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

-- | Given a cell c in a cubical complex, get a subcomplex that includes all 
--   all top-cells that could be adjacent to c (including c). Handy for
--   reducing search problems
cellNhd :: CubeCmplx -> CubeCell -> CubeCmplx
cellNhd cx c = cmplxFilterSpan cx $ vsUnsafe minv maxv
   where minv = (minVert c) `vSub` (vertexVectorUnsafe $ V.replicate (vDim (minVert c)) 1)
         maxv = (maxVert c) `vAdd` (vertexVectorUnsafe $ V.replicate (vDim (minVert c)) 1)

-- | Given a cubical complex all of whose vertices have nonzero coordinates
--   and a corner vertex/cell pair to delete, determine any new corner 
--   vertex/cell pairs, any new top-cells that would be created, and any 
--   potential new top-cells that already were in the complex
cmplxCornerUpdate :: CubeCmplx -> (Vertex, CubeCell)
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
         cvs      = S.filter (\(v1,_) -> (vertToCell v1) `isSubCell` c) $ 
                    cmplxCornersNaive . cmplxAddCells delNhd $ newTops 

--cmplxCornerUpdates cx xss = zip os 
--   where updates    = withStrategy (parBuffer 100 rdeepseq) $
--                      map (cmplxCornerUpdate cx) xss
--         (xs,cs,os) = unzip3 updates


-- | Given a set of (vertex,cell) pairs, return a maximal subset such that 
--   every cell occurs in at most one pair
uniqueCorners :: S.HashSet (Vertex, CubeCell) -> S.HashSet (Vertex, CubeCell)
uniqueCorners vcs = S.fromList . map (\(a,b) -> (b,a)) . M.toList $ 
                  S.foldr step M.empty vcs
   where step (v,c) m = M.insertWith (\v1 v2 -> v1) c v m

-- | Given a complex and a set of corner vertices, remove them one at a time
--   to get a new cubical complex with new corner vertices. Optionally exclude 
--   some corner vertices from consideration.
cmplxCornersDelSerial :: [Vertex]  -- vertices to exclude from corner verts
                         -> (CubeCmplx, S.HashSet (Vertex, CubeCell)) -- input cmplx/corner verts
                         -> (CubeCmplx, S.HashSet (Vertex, CubeCell)) -- output cmplx/new corners
cmplxCornersDelSerial vs (cx,xs) = S.foldr step (cx,xs') corners 
   where vFilt               = S.filter (\(v,_) -> not $ v `elem` vs) 
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
--   to exclude from consideration, iteratively reduce the complex until there
--   are no known non-excluded corner vertices remaining 
cmplxReduceSerial cx xs vs
   | xs == xs' = cx'
   | otherwise = cmplxReduceSerial cx' xs' vs
   where (cx',xs') = cmplxCornersDelSerial vs (cx,xs) 

--cx = vsCmplx $ vsCoordsUnsafe [0,0] [3,2]
--cs = S.fromList $ cmplxCornersNaive $ cx

cx = vsCmplx $ vsCoordsUnsafe (replicate 5 1) (replicate 5 8)
cs = cmplxCornersNaive $ cx

main = print $ cmplxReduceSerial cx cs [vertexUnsafe [1,1,1,1,1], vertexUnsafe [8,8,8,8,8]]


-- | Standard example of finite directed cubical complex: two classes of
--   paths expected in path category
-- cmplxReduceSerial swissFlag (cmplxCorners swissFlag) [vertexUnsafe [1,1], 
-- vertexUnsafe [6,6]]
swissFlag :: CubeCmplx
swissFlag = cmplxDelCells (vsCmplx $ vsCoordsUnsafe [1,1] [6,6]) $
            S.fromList $ [cellUnsafe [2,3] [3,4], cellUnsafe [3,2] [4,3],
                          cellUnsafe [3,3] [4,4], cellUnsafe [4,3] [5,4],
                          cellUnsafe [3,4] [4,5]]

-- | Standard example: four classes of paths expected in path category
--cmplxReduceSerial sqPairFwd (cmplxCorners sqPairFwd) [vertexUnsafe [1,1], 
-- vertexUnsafe [6,6]]
sqPairFwd :: CubeCmplx
sqPairFwd = cmplxDelCells (vsCmplx $ vsCoordsUnsafe [1,1] [6,6]) $
            S.fromList $ [cellUnsafe [2,2] [3,3], cellUnsafe [4,4] [5,5]]

-- | Standard example: three classes of paths expected in path category
--cmplxReduceSerial sqPairBack (cmplxCorners sqPairBack) [vertexUnsafe [1,1], 
-- vertexUnsafe [6,6]]
sqPairBack :: CubeCmplx
sqPairBack = cmplxDelCells (vsCmplx $ vsCoordsUnsafe [1,1] [6,6]) $
             S.fromList $ [cellUnsafe [2,4] [3,5], cellUnsafe [4,2] [5,3]]

-- | Standard example: two classes of paths expected in path category
--cmplxReduceSerial oneTorus3d (cmplxCorners oneTorus3d) [vertexUnsafe [1,1,1], 
-- vertexUnsafe [4,4,2]]
oneTorus3d :: CubeCmplx
oneTorus3d = cmplxDelCells (vsCmplx $ vsCoordsUnsafe [1,1,1] [4,4,2]) $
             S.fromList $ [cellUnsafe [2,2,1] [3,3,2]]

-- | Standard example: three classes of paths expected in path category
--cmplxReduceSerial twoTorus3d (cmplxCorners twoTorus3d) [vertexUnsafe [1,1,1], 
-- vertexUnsafe [4,6,2]]
twoTorus3d = cmplxDelCells (vsCmplx $ vsCoordsUnsafe [1,1,1] [4,6,2]) $
             S.fromList $ [cellUnsafe [2,2,1] [3,3,2], 
                           cellUnsafe [2,4,1] [3,5,2]]

