{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- | Module    :  Math.Topology.CubeCmplx.DPTrace
-- Copyright   :  2014 Michael Misamore 
-- License     :  BSD-style
-- Maintainer  :  m.misamore@gmail.com 
-- Stability   :  experimental 
-- Portability :  portable
--
-- Model directed process traces in a directed cubical framework.
--
module Math.Topology.CubeCmplx.DPTrace (

   -- * Resource requests
   ResId, ReqType(..), ResReq,

   -- * Process Traces
   PID, PTrace, pTrace, pTraces, 

   -- * Modeling contention problems
   ptsAmbReg, ptsForbRegs, ptsCmplx

) where

import Data.Function  (on)
import Data.List      (sort, groupBy, sortBy, transpose, (\\))
import Data.Ord       (comparing)
import Data.Maybe     (fromJust, catMaybes)
import Control.Arrow  ((***))
import Control.Monad  (guard)
import qualified Data.IntMap.Strict as M
   (IntMap, empty, null, filter, lookup, insert, adjust)
import Math.Topology.CubeCmplx.DirCubeCmplx 

-- Resource Requests --

-- | Type for resource IDs.
type ResId = Int

-- | Classical acquire/release notation.
data ReqType = P | V deriving (Show, Eq)

-- | Resource request consists of acquiring/releasing a given resource.
type ResReq = (ReqType, ResId)

-- | Valid list of resource requests.
data ResReqs = ResReqs { reqs :: ![ResReq], parity :: !(M.IntMap Bool) }
instance Eq ResReqs where r1 == r2 = (reqs r1) == (reqs r2)
instance Show ResReqs where show rs = show (reqs rs)

-- | Construct empty list of resource requests.
reqEmpty :: ResReqs
reqEmpty = ResReqs [] M.empty

-- | Given a resource request, add to list of resource requests 
--   if permissible.
(<+>) :: ResReq -> ResReqs -> Maybe ResReqs
r@(t,i) <+> rs = case M.lookup i pty of
                    Nothing -> if t == V
                               then Just $ ResReqs (r : reqs rs) 
                                                   (M.insert i True pty)
                               else Nothing
                    Just False -> if t == V then addElt else Nothing
                    Just True  -> if t == P then addElt else Nothing
   where pty    = parity rs 
         addElt = Just $ ResReqs (r : reqs rs) (M.adjust not i pty)

-- | Fold up list of resource requests, validating pair counts.
reqList :: [ResReq] -> Maybe ResReqs
reqList rs = do rss <- bld rs
                guard (M.null . M.filter (== True) $ parity rss)
                return rss
   where bld rs           = foldr step (Just reqEmpty) rs
         step r (Just rs) = r <+> rs
         step r _         = Nothing


-- Process Traces --

-- | Type for process IDs.
type PID = Int

-- | Process trace is Process ID together with valid list of requests.
data PTrace = PTrace { ptId :: !PID, ptReqs :: !ResReqs } deriving (Show,Eq)

-- | Attempt to build valid process trace from ID and list of requests.
pTrace :: PID -> [ResReq] -> Maybe PTrace
pTrace id rs = do rss <- reqList rs
                  return $ PTrace id rss

-- | Attempt to build list of process traces with default process IDs
-- | from list of lists of requests.
pTraces :: [[ResReq]] -> Maybe [PTrace]
pTraces = sequence . map (uncurry pTrace) . zip [1..]

-- | Given a process trace, output list of associated resource request points
-- | ordered by time.
ptPts :: PTrace -> [ReqPt]
ptPts pt = zipWith3 reqPt (repeat $ ptId pt) rs [1..]
   where rs  = reqs . ptReqs $ pt

-- | Given a process trace, output list of lists of associated resource
--   request points grouped by resource id.
ptPtGrps :: PTrace -> [[ReqPt]]
ptPtGrps pt = groupBy ((==) `on` rpResId) .
              sortBy  (comparing rpResId) $ ptPts pt


-- Resource request points --

-- | Resource request points labeled by requesting process and time.
data ReqPt = ReqPt { pid :: !PID, res :: !ResReq, t :: !T } deriving (Eq)

-- | Pretty print resource request points.
instance Show ReqPt where show p = show (pid p, res p, t p)

-- | Ordered by time.
instance Ord ReqPt where p1 <= p2 = (t p1) <= (t p2) 

-- | Construct a resource request point from a triple.
reqPt :: PID -> ResReq -> T -> ReqPt
reqPt p r t = ReqPt p r t

-- | Fetch resource id for a resource request point.
rpResId :: ReqPt -> ResId
rpResId = snd . res


-- Resource Traces --

-- | Resource trace: process, resource, and list of consumption intervals.
data ResTrace = ResTrace { rtPid :: PID, rtResId :: ResId, ivls :: [[T]] }
instance Show ResTrace where show rt = show (rtPid rt, rtResId rt, ivls rt)

-- | Given a process trace, output associated list of resource traces
--   for that process.
ptResTraces :: PTrace -> [ResTrace]
ptResTraces pt = zipWith3 ResTrace (repeat (ptId pt)) (map rid idgrps) 
                                   (map pairUp idgrps)
   where idgrps   = ptPtGrps pt 
         sg g     = sortBy (comparing t) g
         ts g     = map t (sg g)
         pairUp g = pairUp' (ts g) where
                       pairUp' (a:b:rs) = [[a,b]] ++ pairUp' rs
                       pairUp' []       = []
         rid g    = rpResId $ head g 

-- | Given a list of process traces, output associated flattened list of
--   all associated resource traces.
ptsResTraces :: [PTrace] -> [ResTrace]
ptsResTraces = concatMap ptResTraces 

-- | Group list of resource traces by resource id.
rtGrpRes :: [ResTrace] -> [[ResTrace]]
rtGrpRes rts = groupBy ((==) `on` rtResId) .  sortBy  (comparing rtResId) $ rts


-- Resource Competitions --

-- | Competition for a given resource.
type ResComp = [(PID, [[T]])]

-- | Given a list of process traces, get associated list of resource
--   competitions. In each, two or more processes compete for the same
--   resource.
resComps :: [PTrace] -> [ResComp]
resComps pts = map ts . filter ((>1).length) $ rts 
   where rts  = rtGrpRes . ptsResTraces $ pts
         ts g = zip (map rtPid g) (map ivls g) 

-- | Given a resource competition and an ambient dimension n, determine the 
--   maximal vertex spans it could generate in an ambient complex of dim n.
rcVertSpans :: Int -> ResComp -> [VertSpan]
rcVertSpans n rc = zipWith vsCoordsUnsafe
                   (map (map head) $ sequence mins)
                   (map (map last) $ sequence maxes)
   where pids  = map fst rc
         opids = [1..n] \\ pids
         minOs = zip opids (repeat [[0]])
         maxOs = zip opids (repeat [[maxBound :: T]])
         mins  = map snd . sortBy (comparing fst) $ rc ++ minOs
         maxes = map snd . sortBy (comparing fst) $ rc ++ maxOs


-- Modeling Contention Problems --

-- | Given a list of process traces, determine a minimal vertex span suitable
--   for modeling a resource contention problem.
ptsAmbReg :: [PTrace] -> VertSpan
ptsAmbReg pts = uncurry vsCoordsUnsafe . unzip $ map bds pts 
   where coords = map t . ptPts 
         bds    = ((+(-1)) *** (+1)) . 
                  foldr (\k (m,n) -> (min k m, max k n)) 
                  (maxBound :: T, minBound :: T) . coords  

-- | Given a list of process traces, calculate associated list of 
--   "forbidden regions" which represent resource contention. These regions
--   may overlap to form more complex regions.
ptsForbRegs :: [PTrace] -> [VertSpan]
ptsForbRegs pts = concatMap (rcVertSpans n) . resComps $ pts
   where n = vsDim $ ptsAmbReg pts
 
-- | Given a list of process traces, represent the associated resource
--   contention problem by a finite directed cubical complex.
ptsCmplx :: [PTrace] -> CubeCmplx
ptsCmplx pts = cmplxVertOp cx (vertexUnsafe $ replicate (vsDim reg) 1) vAdd
   where reg = ptsAmbReg pts  
         cx  = foldr (flip cmplxDelVsInt) (vsCmplx reg) (ptsForbRegs pts)

