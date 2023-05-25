{-# LANGUAGE RankNTypes #-}

module Control.Computations.Utils.Clock (
  Clock (..),
  TimeoutFun,
  makeClock,
  realClock,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils (
  diffTime,
 )

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Time.Clock
import qualified System.Timeout as Sys

type TimeoutFun a = TimeSpan -> IO a -> IO (Maybe a)

data Clock = Clock
  { c_currentTime :: IO UTCTime
  , c_timeout :: forall a. TimeSpan -> IO a -> IO (Maybe a)
  , c_sleep :: TimeSpan -> IO ()
  , c_sleepUntil :: UTCTime -> IO ()
  }

makeClock :: IO UTCTime -> (forall a. TimeoutFun a) -> (TimeSpan -> IO ()) -> Clock
makeClock getTime timeoutFun sleepFun =
  Clock
    { c_currentTime = getTime
    , c_timeout = timeoutFun
    , c_sleep = sleepFun
    , c_sleepUntil = \targetTime ->
        do
          currentTime <- getTime
          let sleepSpan = targetTime `diffTime` currentTime
          when (isPositiveTimeSpan sleepSpan) $ sleepFun sleepSpan
    }

realClock :: Clock
realClock = makeClock getCurrentTime timeout sleep
 where
  timeout ts action = Sys.timeout (asMicroseconds ts) action
  sleep ts = threadDelay (asMicroseconds ts)
