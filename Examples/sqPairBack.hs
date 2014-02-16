import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

main = print $ sqPairBack `deepseq` uncurry cmplxReducePar $ sqPairBack

