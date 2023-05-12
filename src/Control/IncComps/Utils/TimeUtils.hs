{-# LANGUAGE DeriveAnyClass #-}

module Control.IncComps.Utils.TimeUtils (
  TimeIntervalType (..),
  truncateTime,
  parseUTCTime,
  unsafeParseUTCTime,
  formatUTCTime,
  formatUTCTimeHiRes,
  diffTime,
  addTime,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.Hashable
import qualified Data.LargeHashable as LH
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import GHC.Generics (Generic)

data TimeIntervalType
  = TimeInterval1s -- you should only use this in tests
  | TimeInterval10s -- you should only use this in tests
  | TimeInterval1min
  | TimeInterval5min
  -- more omitted
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance LH.LargeHashable TimeIntervalType

truncateToNSeconds :: Int -> UTCTime -> UTCTime
truncateToNSeconds (toInteger -> n) t =
  let secs = floor $ toRational $ utcTimeToPOSIXSeconds t
      truncated = n * (secs `div` n)
   in posixSecondsToUTCTime (fromInteger truncated)

intervalToSeconds :: TimeIntervalType -> Int
intervalToSeconds interval =
  case interval of
    TimeInterval1s -> 1
    TimeInterval10s -> 10
    TimeInterval1min -> 60
    TimeInterval5min -> 300

truncateTime :: UTCTime -> TimeIntervalType -> UTCTime
truncateTime t interval = truncateToNSeconds (intervalToSeconds interval) t

timeFormatString :: String
timeFormatString = "%Y-%m-%d %H:%M:%S"

timeFormatStringHiRes :: String
timeFormatStringHiRes = "%Y-%m-%d %H:%M:%S.%q"

parseUTCTime :: MonadFail m => String -> m UTCTime
parseUTCTime = parseTimeM True defaultTimeLocale timeFormatString

unsafeParseUTCTime :: String -> UTCTime
unsafeParseUTCTime s =
  case parseUTCTime s of
    Nothing -> error ("Cannot parse " ++ show s ++ " as UTCTime")
    Just t -> t

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale timeFormatString

formatUTCTimeHiRes :: UTCTime -> String
formatUTCTimeHiRes = formatTime defaultTimeLocale timeFormatStringHiRes

diffTime :: UTCTime -> UTCTime -> TimeSpan
diffTime t1 t2 = nominalDiffTimeSpan (diffUTCTime t1 t2)

addTime :: UTCTime -> TimeSpan -> UTCTime
addTime t ts = asNominalDiffTime ts `addUTCTime` t
