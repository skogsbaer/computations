module Control.Computations.Utils.VirtualTime (
  VirtualTime (..),
  addVirtualTimeSpan,
  getCurrentVirtualTime,
  initVirtualTime,
  setVirtualTimeTo,
  sleepVirtualTimeSpan,
  sleepUntilVirtualTime,
  timeoutInVirtualTime,
  virtualClock,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.ConcUtils
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Maybe (isNothing)
import Data.Time.Clock

newtype VirtualTime = VirtualTime
  { _unVirtualTime :: TVar UTCTime
  }

initVirtualTime :: UTCTime -> IO VirtualTime
initVirtualTime t0 =
  do
    logDebug ("Initialized virtual time with " ++ formatUTCTime' t0)
    VirtualTime <$> newTVarIO t0

sleepVirtualTimeSpan :: VirtualTime -> TimeSpan -> IO ()
sleepVirtualTimeSpan vt@(VirtualTime var) ts =
  do
    t0 <- readTVarIO var
    let tWakeup = t0 `addTimeSpan` ts
    logDebug ("VirtualTime is " ++ formatUTCTime' t0 ++ ", sleeping for " ++ show ts)
    sleepUntilVirtualTime vt tWakeup

sleepUntilVirtualTime :: VirtualTime -> UTCTime -> IO ()
sleepUntilVirtualTime (VirtualTime var) tWakeup =
  do
    logDebug
      ("Waiting until virtual time proceeds to (at least) " ++ formatUTCTime' tWakeup)
    tCur <-
      atomically $
        do
          tCur <- readTVar var
          if tCur >= tWakeup
            then return tCur
            else retry
    logDebug ("Done sleeping until " ++ formatUTCTime' tCur)

virtualClock :: VirtualTime -> Clock
virtualClock vt@(VirtualTime var) =
  -- Don't use makeClock here to avoid race conditions
  Clock
    { c_currentTime = readTVarIO var
    , c_timeout = timeoutInVirtualTime vt
    , c_sleep = sleepVirtualTimeSpan vt
    , c_sleepUntil = sleepUntilVirtualTime vt
    }

getCurrentVirtualTime :: VirtualTime -> IO UTCTime
getCurrentVirtualTime (VirtualTime var) = readTVarIO var

addVirtualTimeSpan :: VirtualTime -> TimeSpan -> IO ()
addVirtualTimeSpan vt ts =
  modifyVirtualTime vt (`addTimeSpan` ts)

modifyVirtualTime :: VirtualTime -> (UTCTime -> UTCTime) -> IO ()
modifyVirtualTime (VirtualTime var) f =
  do
    (tOld, tNew) <-
      atomically $ do
        tOld <- readTVar var
        let !tNew = f tOld
        writeTVar var tNew
        pure (tOld, tNew)
    logDebug ("Virtual time changed from " ++ show tOld ++ " to " ++ show tNew ++ ".")

setVirtualTimeTo :: VirtualTime -> UTCTime -> IO ()
setVirtualTimeTo vt t =
  modifyVirtualTime vt (const t)

timeoutInVirtualTime :: VirtualTime -> TimeSpan -> IO a -> IO (Maybe a)
timeoutInVirtualTime vt@(VirtualTime var) ts action =
  do
    ((`addTimeSpan` ts) -> tCancelTime) <- readTVarIO var
    withAsync (Just <$> action) $ \run ->
      withAsync (sleepAction tCancelTime) $ \sleep ->
        do
          logDebug
            ( "Timeout after "
                ++ show ts
                ++ ", tCancetime="
                ++ formatUTCTime' tCancelTime
            )
          result <- fmap snd (waitAnyCancel [run, sleep])
          logDebug (if isNothing result then "Timeout of action" else "No timeout of action")
          return result
 where
  sleepAction until =
    ignoreThreadKilled' Nothing $
      do
        sleepUntilVirtualTime vt until
        return Nothing
