import Math.Topology.CubeCmplx.DirCubeCmplx
import Math.Topology.CubeCmplx.CornerReduce
import Control.DeepSeq

main = print $ genusTwo3d `deepseq` uncurry cmplxReduce $ genusTwo3d

