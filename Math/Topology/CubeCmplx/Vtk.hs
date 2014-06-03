{-# LANGUAGE OverloadedStrings #-}

-- | Module    :  Math.Topology.CubeCmplx.Vtk
-- Copyright   :  2014 Michael Misamore 
-- License     :  BSD-style
-- Maintainer  :  m.misamore@gmail.com 
-- Stability   :  experimental 
-- Portability :  portable
--
-- Output finite directed cubical complexes in VTK polydata format


-- 1. get all vertices
-- 2. assign labels to vertices in map
-- 3. generate list of polygons per vertex list via map
-- 4. assemble final data structure
-- 5. serialize to file

--module Math.Topology.CubeCmplx.Vtk (
-- ) where

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

-- | Data type for VTK polygons.
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
type Title = BS.ByteString

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
   = VtkPolyDataAscii { title    = t,
                        points   = (pmConv . pointmap $ pd),
                        vertices = filter ((==1).length.getPoly) (polys pd),
                        lines    = filter ((==2).length.getPoly) (polys pd),
                        polygons = filter ((==3).length.getPoly) (polys pd) }
   where pmConv :: M.HashMap Int VtkPoint -> [VtkPoint]
         pmConv m = map snd . sortBy (comparing fst) . M.toList $ m

-- | Marshall a VtkPolyDataAscii to a ByteString to write to a file.
vtkPolyDataAscToBS :: VtkPolyDataAscii -> BS.ByteString
vtkPolyDataAscToBS da = "# vtk DataFile Version 2.0\n" `BS.append`
                        BS.snoc (title da) '\n' `BS.append`
                        "ASCII\n" `BS.append`
                        "DATASET POLYDATA\n" `BS.append`
                        "POINTS " `BS.append` 
                        (BS.pack . show . length $ points da) `BS.append`
                        " float\n" `BS.append`
                        BS.concat (map vtkPointAsc (points da)) `BS.append`
                        "\n" `BS.append` 
                        vertsOut `BS.append` linesOut `BS.append` polysOut
   where vertLen   = length $ vertices da 
         totalVLen = vertLen + (sum (map (length.getPoly) $ vertices da))
         lineLen   = length $ lines da 
         totalLLen = lineLen + (sum (map (length.getPoly) $ lines da))
         polyLen   = length $ polygons da
         totalPLen = polyLen + (sum (map (length.getPoly) $ polygons da))
         vertsOut  = if vertLen > 0 then
                        "VERTICES " `BS.append`
                        (BS.pack . show $ vertLen) `BS.append` " " `BS.append`
                        (BS.pack . show $ totalVLen) `BS.append` "\n" `BS.append`
                        BS.concat (map vtkPolyAsc (vertices da)) `BS.append` "\n" 
                     else BS.empty
         linesOut  = if lineLen > 0 then
                        "LINES " `BS.append`
                        (BS.pack . show $ lineLen) `BS.append` " " `BS.append`
                        (BS.pack . show $ totalLLen) `BS.append` "\n" `BS.append`
                        BS.concat (map vtkPolyAsc (lines da)) `BS.append` "\n" 
                     else BS.empty
         polysOut  = if polyLen > 0 then                      
                        "POLYGONS " `BS.append`
                        (BS.pack . show $ polyLen) `BS.append` " " `BS.append`
                        (BS.pack . show $ totalPLen) `BS.append` "\n" `BS.append`
                        BS.concat (map vtkPolyAsc (polygons da)) `BS.append` "\n"
                     else BS.empty

-- | Given a cubical complex, take its 2-skeleton and render a VTK ascii file
--   in polydata format that represents this complex.
vtkPolyDataAscBS :: Title -> CubeCmplx -> BS.ByteString
vtkPolyDataAscBS t cx = vtkPolyDataAscToBS $ 
                        vtkPolyDataAsc t (vtkPolyData cx)  

-- | Type for filenames.
type FileName = String

-- | Given a filename, title, and cubical complex, take the 2-skeleton, render
--   to VTK polydata ascii format, and write to storage.
vtkPolyAscFile :: FileName -> Title -> CubeCmplx -> IO ()
vtkPolyAscFile f t cx = BS.writeFile f $ vtkPolyDataAscBS t cx

