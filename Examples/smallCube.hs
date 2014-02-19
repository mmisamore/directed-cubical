import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

cx = vsCmplx $ vsCoordsUnsafe (replicate 3 1) (replicate 3 10)

main = print $ cx `deepseq` cmplxReduce cx 
         [vsVert $ vertexUnsafe (replicate 3 1),
          vsVert $ vertexUnsafe (replicate 3 10)]

