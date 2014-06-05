{-# LANGUAGE OverloadedStrings #-}

-- | Module    :  Math.Topology.CubeCmplx.Vtk
-- Copyright   :  2014 Michael Misamore 
-- License     :  BSD-style
-- Maintainer  :  m.misamore@gmail.com 
-- Stability   :  experimental 
-- Portability :  portable
--
-- Output finite directed cubical complexes in VTK polydata format.

module Math.Topology.CubeCmplx.Vtk (
   -- * Write to ByteString.
   vtkPolyAscBS, vtkPolyAscBSs,
   
   -- * Write to file.
   vtkPolyAscFile, vtkPolyAscFiles
) where

import Prelude hiding (lines)
import Data.Ord (comparing)
import Data.List (nub, sortBy)
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.HashMap.Strict as M (HashMap, fromList, lookupDefault, toList)
import qualified Data.HashSet as S (HashSet, toList)
import qualified Data.ByteString.Char8 as BS 
   (ByteString, pack, snoc, init, append, concat, writeFile, empty)
import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce

-- | Data type for VTK points.
newtype VtkPoint = VtkPoint { getPt :: [Int] } deriving (Eq, Show)
instance Hashable VtkPoint where hashWithSalt s p = s + hash (getPt p)

-- | Convert a vertex to a VTK point.
vtkPtVert :: Vertex -> VtkPoint
vtkPtVert = VtkPoint . map fromEnum . vertexToList 

-- | Data type for VTK polygons (vertices, lines, simplices, etc.)
newtype VtkPoly = VtkPoly { getPoly :: [Int] } deriving (Eq, Show)

-- | Representation of the polydata format.
data VtkPolyData = VtkPolyData { pointmap :: !(M.HashMap Int VtkPoint), 
                                 polys    :: ![VtkPoly] } deriving (Show)

-- | Given cubical complex, take its 2-skeleton and return the corresponding
--   VTK polydata structure.
vtkPolyData :: CubeCmplx -> VtkPolyData 
vtkPolyData cx = VtkPolyData bij (intRep tri m)
   where tri = cmplx2Triang cx 
         m   = vertMap tri         
         bij = M.fromList . map (\(x,y) -> (y,x)) $ M.toList m
 
         intRep :: S.HashSet [Vertex] -> M.HashMap VtkPoint Int -> [VtkPoly]
         intRep s m = map (VtkPoly . repList) $ S.toList s
            where repList vs = map (\v -> M.lookupDefault 0 (vtkPtVert v) m) vs
 
         vertMap :: S.HashSet [Vertex] -> M.HashMap VtkPoint Int
         vertMap s = M.fromList keyed
            where keyed = zip (map vtkPtVert . nub . concat $ S.toList s) [0..]

-- | Render a VTK point in ascii file format.
vtkPointAsc :: VtkPoint -> BS.ByteString
vtkPointAsc pt = padZeroes `BS.append` (BS.snoc (BS.pack . render $ pt) '\n')
   where render pt = init $ foldr (\i b -> (show i) ++ " " ++ b) "" (getPt pt)
         padZeroes = BS.concat $ replicate (3 - (length $ getPt pt)) "0 "

-- | Render a VTK polygon in ascii file format.
vtkPolyAsc :: VtkPoly -> BS.ByteString 
vtkPolyAsc p = BS.snoc (BS.pack . render $ p) '\n'
   where render p = (len ++) . init $ 
                    foldr (\i b -> (show i) ++ " " ++ b) "" (getPoly p)
         len      = show (length $ getPoly p) ++ " "

-- | Type for file titles.
type Title = String 

-- | Representation of VTK polydata ascii file.
data VtkPolyDataAscii = VtkPolyDataAscii { title     :: !Title, 
                                           points    :: ![VtkPoint],
                                           vertices  :: ![VtkPoly], 
                                           lines     :: ![VtkPoly],
                                           polygons  :: ![VtkPoly]
                                           -- other file contents 
                                         } deriving (Show)

-- | Given a VtkPolyData, marshall it to a VtkPolyDataAscii.
vtkPolyDataAsc :: Title -> VtkPolyData -> VtkPolyDataAscii
vtkPolyDataAsc t pd 
   = VtkPolyDataAscii { title    = t, points   = pmConv . pointmap $ pd,
                        vertices = v', lines   = l', polygons = p' }
   where pmConv :: M.HashMap Int VtkPoint -> [VtkPoint]
         pmConv m = map snd . sortBy (comparing fst) . M.toList $ m
         (v',l',p') = foldr (\(len,e) (v,l,p) -> 
                               case len of
                                  1 -> (e:v,l,p)
                                  2 -> (v,e:l,p)
                                  3 -> (v,l,e:p)
                                  _ -> (v,l,p)) ([],[],[]) lps 
         lps = zip (map (length.getPoly) (polys pd)) (polys pd)

-- | Marshall a VtkPolyDataAscii to a ByteString to write to a file.
vtkPolyDataAscToBS :: VtkPolyDataAscii -> BS.ByteString
vtkPolyDataAscToBS da = BS.concat ["# vtk DataFile Version 2.0\n", 
                        BS.snoc (BS.pack $ title da) '\n',
                        "ASCII\n", "DATASET POLYDATA\n", "POINTS ",
                        (BS.pack . show . length $ points da), " float\n",
                        BS.concat (map vtkPointAsc (points da)), "\n",
                        vertsOut, linesOut, polysOut]
   where [vs, ls, ps] = [vertices da, lines da, polygons da] 
         [vertLen, lineLen, polyLen] = [length $ vs, length $ ls, length $ ps]
         totalVLen = vertLen + sum (map (length.getPoly) vs)
         totalLLen = lineLen + sum (map (length.getPoly) ls)
         totalPLen = polyLen + sum (map (length.getPoly) ps)
         vertsOut  = if vertLen > 0 then
                        BS.concat ["VERTICES ", (BS.pack . show $ vertLen), 
                        " ", (BS.pack . show $ totalVLen), "\n", 
                        BS.concat (map vtkPolyAsc vs), "\n"]
                     else BS.empty
         linesOut  = if lineLen > 0 then
                        BS.concat ["LINES ", (BS.pack . show $ lineLen),
                        " ", (BS.pack . show $ totalLLen), "\n",
                        BS.concat (map vtkPolyAsc ls), "\n"]
                     else BS.empty
         polysOut  = if polyLen > 0 then                      
                        BS.concat ["POLYGONS ", (BS.pack . show $ polyLen),
                        " ", (BS.pack . show $ totalPLen), "\n",
                        BS.concat (map vtkPolyAsc ps), "\n"]
                     else BS.empty

-- | Given a cubical complex, take its 2-skeleton and render a VTK ascii file
--   in polydata format that represents this complex.
vtkPolyAscBS :: Title -> CubeCmplx -> BS.ByteString
vtkPolyAscBS t cx = vtkPolyDataAscToBS $ vtkPolyDataAsc t (vtkPolyData cx)  

-- | Type for filenames.
type FileName = String

-- | Given a filename, title, and cubical complex, take the 2-skeleton, render
--   to VTK polydata ascii format, and write to storage.
vtkPolyAscFile :: FileName -> Title -> CubeCmplx -> IO ()
vtkPolyAscFile f t cx = BS.writeFile f $ vtkPolyAscBS t cx

-- | Given a base filename, base title, and list of cubical complexes, generate
--   a list of named bytestrings of VTK ascii files associated to these
--   complexes. Filenames and titles are appended with integers for 
--   compatibility with tools like ParaView.
vtkPolyAscBSs :: FileName -> Title -> [CubeCmplx] -> [(FileName, BS.ByteString)]
vtkPolyAscBSs f t cxs = zip files $ map (uncurry vtkPolyAscBS) $ zip titles cxs 
   where files  = map (++ ".vtk") $ zipWith (++) (repeat f) (map show [1..])
         titles = zipWith (++) (repeat t) (map show [1..])

-- | Same as vtkPolyAscBSs, but write the resulting files.
vtkPolyAscFiles :: FileName -> Title -> [CubeCmplx] -> IO ()
vtkPolyAscFiles f t cxs = mapM_ (uncurry BS.writeFile) $ vtkPolyAscBSs f t cxs

