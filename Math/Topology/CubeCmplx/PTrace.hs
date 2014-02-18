{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- | Module    :  Math.Topology.DirCubeCmplx.PTrace
-- Copyright   :  2014 Michael Misamore 
-- License     :  BSD-style
-- Maintainer  :  m.misamore@gmail.com 
-- Stability   :  experimental 
-- Portability :  portable
--
-- Generate forbidden regions from process traces. This module probably deserves
-- more attention and a redesign for use in serious applications, so we
-- minimize surface area for now.

module PTrace (
   ReqType(..), PTrace, pTraces, forbRegs 
)
where

import DirCubeCmplx -- support for cubical complexes
import Safe           (headMay)
import Data.List      (group, sort, sortBy, nub)
import Data.Ord       (comparing)
import Data.Maybe     (fromJust,catMaybes)
import Control.Arrow  ((&&&))
import Control.Monad  (guard,liftM)
import qualified Data.IntMap.Strict as M
   (IntMap, empty, null, filter, lookup, insert, adjust)
import Test.QuickCheck

-- | Type for resource IDs.
type ResId = Int

-- | Classical acquire/release notation.
data ReqType = P | V deriving (Show, Eq)

-- | Resource request consists of acquiring/releasing a given resource.
data ResReq = ResReq { reqType :: !ReqType, resId :: !ResId }
   deriving (Eq)
instance Show ResReq where show r = show (reqType r, resId r)

-- | Construct resource request from type and integer id.
resReq :: (ReqType, Int) -> ResReq
resReq (t, id) = ResReq { reqType = t, resId = id }

-- | List of resource requests and acquire/release parity per resource.
data ResReqs = ResReqs { fromResReqs :: ![ResReq], resParity :: M.IntMap Bool }
   deriving (Show, Eq)

-- | Construct empty list of resource requests.
emptyResReqs :: ResReqs
emptyResReqs = ResReqs [] M.empty

-- | Given a resource request, add it to list of resource requests if 
--   permissible.
(<+>) :: ResReq -> ResReqs -> Maybe ResReqs
r <+> rs = case M.lookup (resId r) (resParity rs) of
              Nothing -> if reqType r == V
                         then Just $ ResReqs (r:fromResReqs rs) $
                                     M.insert (resId r) True (resParity rs)
                         else Nothing
              Just False -> if reqType r == V then addElt else Nothing
              Just True  -> if reqType r == P then addElt else Nothing
   where addElt = Just $ ResReqs (r:fromResReqs rs) $
                  M.adjust not (resId r) (resParity rs)

-- | Fold up list of resource requests, validating pair counts.
resReqs :: [(ReqType, Int)] -> Maybe ResReqs
resReqs rs = do rss <- bld rs
                guard (M.null . M.filter (== True) $ resParity rss)
                return rss
   where bld rs           = foldr step (Just emptyResReqs) rs
         step r (Just rs) = (resReq r) <+> rs
         step r _         = Nothing

-- | Get list of resource IDs for request list.
resources :: ResReqs -> [ResId]
resources = nub . map resId . fromResReqs 

-- | Get acquire/release pattern for a given resource ID in a
-- | list of resource requests.
filterRes :: ResId -> ResReqs -> [ReqType]
filterRes i = map reqType . filter (\r -> resId r == i) . fromResReqs 

-- | Type for process IDs.
type ProcId = Int

-- | Resource request points labeled by requesting process and time.
data ResReqPt = ResReqPt { procId :: !ProcId, res :: !ResReq, t :: !T }

-- | Pretty print resource request points.
instance Show ResReqPt where show p = "{p = "  ++ show (procId p) ++ 
                                      ", t = " ++ show (t p)      ++
                                      ", r = " ++ show (res p)    ++ "}"

-- | Equivalence on process ID and resource ID but not time.
instance Eq ResReqPt where
  a == b = (procId a, resId $ res a) == (procId b, resId $ res b)

-- | Order by process ID, resource ID, and then time.
instance Ord ResReqPt where
   compare a b = if x /= EQ then x
                 else if y /= EQ then y else compare (t a) (t b) 
      where x = compare (procId a) (procId b)
            y = compare (resId $ res a) (resId $ res b)

-- | Construct a resource request point from a triple.
resReqPt :: ProcId -> ResReq -> T -> ResReqPt
resReqPt p r t = ResReqPt { procId = p, res = r, t = t }

-- | Process trace is Process ID together with valid list of requests.
data PTrace = PTrace { pid :: !ProcId, reqList :: !ResReqs } deriving (Show)

-- | Attempt to build valid process trace from ID and list of requests.
pTrace :: Int -> [(ReqType, Int)] -> Maybe PTrace
pTrace id rs = do rss <- resReqs rs
                  return $ PTrace { pid = id, reqList = rss }

-- | Attempt to build list of process traces with default process IDs
-- | from list of lists of requests.
pTraces :: [[(ReqType, Int)]] -> Maybe [PTrace]
pTraces = sequence . map (uncurry pTrace) . zip [1..]

-- | Given a process trace, output list of resource request points
-- | ordered by time.
pTracePts :: PTrace -> [ResReqPt]
pTracePts pt = zipWith3 resReqPt (repeat (pid pt)) rs [1..]
   where rs  = fromResReqs . reqList $ pt

-- | Given list of process traces, output flattened list of resource
-- | request points.
pTracesPts :: [PTrace] -> [ResReqPt]
pTracesPts = concatMap pTracePts

-- | Resource trace: process, resource, and list of consumption intervals.
data ResTrace =
   ResTrace { pId :: !ProcId, rId :: !ResId, times :: ![(T,T)] }
   deriving (Show)
instance Ord ResTrace where compare a b = compare (rId a) (rId b)
instance Eq  ResTrace where a == b = (rId a) == (rId b)

-- | Given list of ResReqPts for given (process,resource) pair
-- | already sorted by time, produce pair of process/resource and list
-- | of time intervals.
intervals :: [ResReqPt] -> Maybe ResTrace
intervals rs = do r <- headMay rs
                  return $ ResTrace (procId r) (resId $ res r) (f rs)
   where f (a:b:bs) = (t a, t b) : f bs
         f _        = []

-- | Given list of process traces, determine list of resources and their
-- | consumption intervals across all processes.
allIntervals :: [PTrace] -> [ResTrace]
allIntervals = maybe [] id . sequence . map intervals . 
               group . sort . pTracesPts 

-- | Given list of process traces, determine all associated (possibly
-- | overlapping) forbidden regions.
forbRegs :: [PTrace] -> [VertSpan]
forbRegs = catMaybes . concatMap forbReg . resComps
   where resComps = map (sortBy $ comparing pId) . group . sort . allIntervals
         forbReg  = map (uncurry vsCoords . (map fst &&& map snd)) .
                    sequence . map times

