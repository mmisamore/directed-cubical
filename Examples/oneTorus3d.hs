import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

main = print $ oneTorus3d `deepseq` uncurry cmplxReduce $ oneTorus3d 

