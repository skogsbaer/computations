{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.IncComps.FlowImpls.TimeSrc (
  TimeSrcReq (..),
  TimeSrc,
  initTimeSrc,
  closeTimeSrc,
  withTimeSrc,
  defaultTimeSrcId,
  withDefaultTimeSrc,
  compGetTime,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.CompEngine.Types
import Control.IncComps.Utils.Clock
import Control.IncComps.Utils.ConcUtils
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Types
import Control.IncComps.Utils.VirtualTime

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Time.Clock
import Test.Framework

data TimeSrcReq a where
  GetTime :: TimeIntervalType -> TimeSrcReq UTCTime

newtype TimeKey = TimeKey {unTimeKey :: TimeIntervalType}
  deriving (Eq, Ord, Show, Hashable, LH.LargeHashable)

newtype TimeVer = TimeVer {unTimeVer :: UTCTime}
  deriving (Eq, Ord, Show, Hashable, LH.LargeHashable)

type TimeDep = Dep TimeKey TimeVer

data TimeSrc = TimeSrc
  { tcs_ident :: CompSrcInstanceId
  , tcs_thread :: Async ()
  , tcs_truncatedTimes :: TVar (TimeIntervalType -> UTCTime)
  , tcs_state :: TVar (Map TimeIntervalType UTCTime)
  }

defaultTimeSrcInstanceId :: CompSrcInstanceId
defaultTimeSrcInstanceId = CompSrcInstanceId "defaultTimeSrc"

defaultTimeSrcId :: TypedCompSrcId TimeSrc
defaultTimeSrcId = typedCompSrcId (Proxy @TimeSrc) defaultTimeSrcInstanceId

compGetTime :: TimeIntervalType -> CompM UTCTime
compGetTime t = compSrcReq defaultTimeSrcId (GetTime t)

initTimeSrc :: CompSrcInstanceId -> Clock -> IO TimeSrc
initTimeSrc ident clock = do
  startTimes <- getTruncatedTimes
  timesVar <- newTVarIO startTimes
  lastStateVar <- newTVarIO Map.empty
  thread <- async (loop timesVar startTimes)
  pure (TimeSrc ident thread timesVar lastStateVar)
 where
  loop timesVar curTimes = do
    newTimes <- getTruncatedTimes
    unless (all (\ti -> newTimes ti == curTimes ti) [minBound .. maxBound]) $
      atomically (writeTVar timesVar newTimes)
    let now = newTimes TimeInterval1s
    c_sleepUntil clock (now `addTimeSpan` seconds 1)
    loop timesVar newTimes
  getTruncatedTimes = do
    t <- c_currentTime clock
    pure (truncateTime t)

closeTimeSrc :: TimeSrc -> IO ()
closeTimeSrc tcs = cancel (tcs_thread tcs)

withTimeSrc :: CompSrcInstanceId -> Clock -> (TimeSrc -> IO a) -> IO a
withTimeSrc ident clock =
  bracket (initTimeSrc ident clock) closeTimeSrc

withDefaultTimeSrc :: (TimeSrc -> IO a) -> IO a
withDefaultTimeSrc = withTimeSrc defaultTimeSrcInstanceId realClock

instance CompSrc TimeSrc where
  type CompSrcReq TimeSrc = TimeSrcReq
  type CompSrcKey TimeSrc = TimeKey
  type CompSrcVer TimeSrc = TimeVer
  compSrcInstanceId = tcs_ident
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

waitChangesImpl :: TimeSrc -> STM (HashSet TimeDep)
waitChangesImpl tcs = do
  truncTimes <- readTVar (tcs_truncatedTimes tcs)
  lastState <- readTVar (tcs_state tcs)
  let outdated =
        flip Map.mapMaybeWithKey lastState $ \interval old ->
          let new = truncTimes interval
           in if new == old then Nothing else Just new
  when (Map.null outdated) retry
  let newState = Map.unionWith max outdated lastState
      changedDeps =
        HashSet.fromList $
          map (\(k, v) -> Dep (TimeKey k) (TimeVer v)) (Map.toList outdated)
  writeTVar (tcs_state tcs) newState
  pure changedDeps

executeImpl :: TimeSrc -> TimeSrcReq a -> IO (HashSet TimeDep, Fail a)
executeImpl tcs (GetTime interval) = atomically $ do
  timesFun <- readTVar (tcs_truncatedTimes tcs)
  let time = timesFun interval
      dep = Dep (TimeKey interval) (TimeVer time)
  modifyTVar' (tcs_state tcs) (Map.insertWith min interval time)
  pure (HashSet.singleton dep, Ok time)

unregisterImpl :: TimeSrc -> HashSet TimeKey -> IO ()
unregisterImpl tcs keys = atomically $ do
  forM_ keys $ \interval -> modifyTVar' (tcs_state tcs) (Map.delete (unTimeKey interval))

--
-- Tests
--

withTestTimeSrc :: UTCTime -> (VirtualTime -> TimeSrc -> IO a) -> IO a
withTestTimeSrc t0 f =
  do
    vt <- initVirtualTime t0
    let clock = virtualClock vt
    withTimeSrc "testTimeSrc" clock $ \dif ->
      f vt dif

test_timeCompSrc :: IO ()
test_timeCompSrc =
  timeoutFail "TimeSrc should be prompt" (seconds 1) $
    withTestTimeSrc t0 $ \vt dif ->
      do
        let allIntervals = [minBound .. maxBound]
        forM_ allIntervals $ \int ->
          do
            (deps, res) <- compSrcExecute dif (GetTime int)
            let expected = truncateTime t0 int
            assertEqual (HashSet.singleton (Dep (TimeKey int) (TimeVer expected))) deps
            assertEqual (Ok expected) res

        -- no change as time stays constant
        changes <- atomically $ fmap Some (compSrcWaitChanges dif) `orElse` pure None
        assertEqual None changes

        addVirtualTimeSpan vt (seconds 1)
        changes <- atomically $ compSrcWaitChanges dif
        let t1 = addTimeSpan t0 (seconds 1)
            expected = HashSet.singleton (Dep (TimeKey TimeInterval1s) (TimeVer t1))
        assertEqual expected changes

        addVirtualTimeSpan vt (seconds 1)
        changes <- atomically $ compSrcWaitChanges dif
        let t2 = addTimeSpan t0 (seconds 2)
            expected = HashSet.singleton (Dep (TimeKey TimeInterval1s) (TimeVer t2))
        assertEqual expected changes
 where
  t0 = unsafeParseUTCTime "2023-04-28 23:32:01"
