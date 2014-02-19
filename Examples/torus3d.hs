import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

main = print $ torus3d `deepseq` uncurry cmplxReduce $ torus3d 

