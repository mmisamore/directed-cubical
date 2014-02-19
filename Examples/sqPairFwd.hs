import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

main = print $ sqPairFwd `deepseq` uncurry cmplxReduce $ sqPairFwd 

