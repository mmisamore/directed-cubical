import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

main = print $ twoTorus3d `deepseq` uncurry cmplxReduce $ twoTorus3d 

