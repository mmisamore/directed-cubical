import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

cx = vsCmplx $ vsCoordsUnsafe (replicate 5 1) (replicate 5 6)

main = print $ cx `deepseq` cmplxReduce cx 
         [vsVert $ vertexUnsafe (replicate 5 1),
          vsVert $ vertexUnsafe (replicate 5 6)]

