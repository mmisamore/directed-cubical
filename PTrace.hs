{-- Description: Types and functions for Process Traces
    Author:      Mike Misamore
    Updated:     4 October 2013
--}

{-# LANGUAGE BangPatterns #-}

module PTrace (ResId, ReqType, ResReq, resReq, ResReqs, resReqs, resources,
               filterRes, ProcId, PTrace, pTrace, pTraces, forbRegs, ambReg) 
where

import Cubical -- support for cubical complexes
import Safe           (headMay)
import Data.List      (group, sort, sortBy, nub)
import Data.Ord       (comparing)
import Data.Maybe     (fromJust,catMaybes)
import Control.Arrow  ((&&&))
import Control.Monad  (guard,liftM)
import qualified Data.IntMap.Strict as M
   (IntMap, empty, null, filter, lookup, insert, adjust)
import Test.QuickCheck

-- | Type for resource IDs
type ResId = Int

-- | Classical acquire/release notation
data ReqType = P | V deriving (Show, Eq)

-- | Resource request consists of acquiring/releasing a given resource
data ResReq = ResReq { reqType :: !ReqType, resId :: !ResId }
   deriving (Eq)
instance Show ResReq where show r = show (reqType r, resId r)

-- | Construct resource request from type and integer id
resReq :: (ReqType, Int) -> ResReq
resReq (t, id) = ResReq { reqType = t, resId = id }

-- | List of resource requests and acquire/release parity per resource
data ResReqs = ResReqs { fromResReqs :: ![ResReq], resParity :: M.IntMap Bool }
   deriving (Show, Eq)

-- | Construct empty list of resource requests
emptyResReqs :: ResReqs
emptyResReqs = ResReqs [] M.empty

-- | Given a resource request add it to list of resource requests if permissible
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

-- | Fold up list of resource requests, validating pair counts
resReqs :: [(ReqType, Int)] -> Maybe ResReqs
resReqs rs = do rss <- bld rs
                guard (M.null . M.filter (== True) $ resParity rss)
                return rss
   where bld rs = foldr step (Just emptyResReqs) rs
         step r (Just rs) = (resReq r) <+> rs
         step r _ = Nothing

-- | Get list of resource IDs for request list
resources :: ResReqs -> [ResId]
resources = nub . map resId . fromResReqs 

-- | Get acquire/release pattern for a given resource ID in a
-- | list of resource requests
filterRes :: ResId -> ResReqs -> [ReqType]
filterRes i = map reqType . filter (\r -> resId r == i) . fromResReqs 

-- | Type for process IDs
type ProcId = Int

-- | Resource request points labeled by requesting process and time
data ResReqPt = ResReqPt { procId :: !ProcId, res :: !ResReq, t :: !Time }

-- | Pretty print resource request points
instance Show ResReqPt where show p = "{p = "  ++ show (procId p) ++ 
                                      ", t = " ++ show (t p)      ++
                                      ", r = " ++ show (res p)    ++ "}"

-- | Equivalence on process ID and resource ID but not time
instance Eq ResReqPt where
  a == b = (procId a, resId $ res a) == (procId b, resId $ res b)

-- | Order by process ID, resource ID, and then time
instance Ord ResReqPt where
   compare a b = if x /= EQ then x
                 else if y /= EQ then y else compare (t a) (t b) 
      where x = compare (procId a) (procId b)
            y = compare (resId $ res a) (resId $ res b)

-- | Construct a resource request point from a triple
resReqPt :: ProcId -> ResReq -> Time -> ResReqPt
resReqPt p r t = ResReqPt { procId = p, res = r, t = t }

-- | Process trace is Process ID together with valid list of requests
data PTrace = PTrace { pid :: !ProcId, reqList :: !ResReqs } deriving (Show)

-- | Attempt to build valid process trace from ID and list of requests
pTrace :: Int -> [(ReqType, Int)] -> Maybe PTrace
pTrace id rs = do rss <- resReqs rs
                  return $ PTrace { pid = id, reqList = rss }

-- | Attempt to build list of process traces with default process IDs
-- | from list of lists of requests
pTraces :: [[(ReqType, Int)]] -> Maybe [PTrace]
pTraces = sequence . map (uncurry pTrace) . zip [0..]

-- | Given a process trace, output list of resource request points
-- | ordered by time
pTracePts :: PTrace -> [ResReqPt]
pTracePts pt = zipWith3 resReqPt (repeat (pid pt)) rs [0..]
   where rs  = fromResReqs . reqList $ pt

-- | Given list of process traces, output flattened list of resource
-- | request points
pTracesPts :: [PTrace] -> [ResReqPt]
pTracesPts = concatMap pTracePts

-- | Resource trace: process, resource, and list of consumption intervals
data ResTrace =
   ResTrace { pId :: !ProcId, rId :: !ResId, times :: ![(Time, Time)] }
   deriving (Show)
instance Ord ResTrace where compare a b = compare (rId a) (rId b)
instance Eq  ResTrace where a == b = (rId a) == (rId b)

-- | Given list of ResReqPts for given (process,resource) pair
-- | already sorted by time, produce pair of process/resource and list
-- | of time intervals
intervals :: [ResReqPt] -> Maybe ResTrace
intervals rs = do r <- headMay rs
                  return $ ResTrace (procId r) (resId $ res r) (f rs)
   where f (a:b:bs) = (t a, t b) : f bs
         f _        = []

-- | Given list of process traces, determine list of resources and their
-- | consumption intervals across all processes
allIntervals :: [PTrace] -> [ResTrace]
allIntervals = maybe [] id . sequence . map intervals . 
               group . sort . pTracesPts 

-- | Given list of process traces, determine all associated (possibly
-- | overlapping) forbidden regions
forbRegs :: [PTrace] -> [VertSpan]
forbRegs = catMaybes . concatMap forbReg . resComps
   where resComps = map (sortBy $ comparing pId) . group . sort . allIntervals
         forbReg  = map (uncurry vertSpanFromCoords . (map fst &&& map snd))
                      . sequence . map times


-- BEGIN TESTING CODE --

-- Generate acquire/release pair for a random resource
genPair :: Gen [(ReqType, ResId)]
genPair = do i <- arbitrary `suchThat` (>= 0)
             return [(P, i), (V, i)]

-- Generate permutation of first n ints of a given length
genPermute :: Int -> Gen [Int]
genPermute tot = genPermute' tot tot $ return []
   where genPermute' tot len l = 
            if len == 0 then l
            else do x  <- choose (0, tot-1)
                    lu <- l
                    if x `elem` lu 
                    then genPermute' tot len l 
                    else genPermute' tot (len-1) (return $ x:lu)

-- Generate list of pairs which might represent valid resource
-- request list 
genPairList :: Gen [(ReqType, ResId)]
genPairList = do i  <- choose (1,20)
                 vs <- liftM concat $ vectorOf i genPair
                 gs <- genPermute (2*i)
                 let f g b = (vs !! g) : b
                 return $ foldr f [] gs

-- Generate random instances of resource request list
genResReqs :: Gen ResReqs
genResReqs = frequency [(100,rs), (1,return emptyResReqs)]
   where rs = do ps <- genPairList
                 if resReqs ps == Nothing then genResReqs
                 else return . fromJust $ resReqs ps

-- Random list of resource requests
instance Arbitrary ResReqs
   where arbitrary = genResReqs

-- Given list of P's and V's, check for correct pairing
propPV :: [ReqType] -> Bool
propPV []       = True
propPV [_]      = False
propPV (P:V:rs) = propPV rs
propPV _        = False

-- Check whether a list of res requests correctly pairs off resources
propResReqValid :: ResReqs -> Bool 
propResReqValid rs = and [propPV $ filterRes i rs | i <- resources rs]

-- Generate random process trace
genPTrace :: Gen PTrace
genPTrace = do id <- choose (0,20)
               rs  <- genResReqs
               return $ PTrace {pid = id, reqList = rs}
 
-- Generate random list of forbidden regions, mainly for assuring above
-- code will not crash
genForbRegs :: Gen [VertSpan]
genForbRegs = do n   <- choose (1,10)
                 pts <- vectorOf n genPTrace
                 return $ forbRegs pts 

-- | Given list of vertex spans, determine minimal ambient cubical span
ambReg :: [VertSpan] -> VertSpan
ambReg = fromJust . uncurry vertSpanFromCoords . (minV &&& maxV) 
   where minV = map minimum . transpose . map vertSpanFst 
         maxV = map maximum . transpose . map vertSpanSnd
