module Control.IncComps.Utils.TimeSpan (
  TimeSpan,
  zeroTime,
  isPositiveTimeSpan,
  isInRange,
  nominalDiffTimeSpan,
  picoseconds,
  nanoseconds,
  microseconds,
  microseconds',
  milliseconds,
  milliseconds',
  seconds,
  seconds',
  doubleSeconds,
  minutes,
  hours,
  days,
  years,
  asNominalDiffTime,
  asMicroseconds,
  asMilliseconds,
  asSeconds,
  asSeconds',
  asMinutes,
  asHours,
  asDays,
  diffTimeSpan,
) where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Hashable
import Data.LargeHashable
import Data.Time.Clock
import GHC.Generics (Generic)

-- | Represents a time span with microsecond resolution.
newtype TimeSpan = TimeSpan {unTimeSpan :: Integer}
  deriving newtype (Eq, Ord, Num, Hashable)
  deriving stock (Generic)

instance LargeHashable TimeSpan

instance Show TimeSpan where
  showsPrec _ (TimeSpan t)
    | t < 0 = showString "-" . shows (TimeSpan (-t))
    | us == 0 && ms == 0 && s == 0 && mins == 0 && h == 0 && ds == 0 =
        showString "0us"
    | otherwise =
        possiblyShow ds "days"
          . possiblyShow h "h"
          . possiblyShow mins "min"
          . possiblyShow s "s"
          . possiblyShow ms "ms"
          . possiblyShow us "us"
   where
    possiblyShow i unit =
      if i == 0 then id else shows i . showString unit
    us = t `mod` 1000
    ms = t `div` 1000 `mod` 1000
    s = t `div` (1000 ^ two) `mod` 60
    mins = t `div` (1000 ^ two * 60) `mod` 60
    h = t `div` (1000 ^ two * 60 ^ two) `mod` 24
    ds = t `div` (1000 ^ two * 60 ^ two * 24)
    two :: Integer
    two = 2

zeroTime :: TimeSpan
zeroTime = TimeSpan 0

isPositiveTimeSpan :: TimeSpan -> Bool
isPositiveTimeSpan (TimeSpan t) = t > 0

isInRange :: TimeSpan -> TimeSpan -> Bool
isInRange range obj = obj < range && obj > -range

nominalDiffTimeSpan :: NominalDiffTime -> TimeSpan
nominalDiffTimeSpan dt = nanoseconds (truncate (dt * 1000 * 1000 * 1000))

picoseconds :: Integer -> TimeSpan
picoseconds = TimeSpan . (`div` (1000 * 1000))

nanoseconds :: Integer -> TimeSpan
nanoseconds = TimeSpan . (`div` 1000)

microseconds :: Int -> TimeSpan
microseconds = TimeSpan . fromIntegral

microseconds' :: Integer -> TimeSpan
microseconds' = TimeSpan

milliseconds :: Int -> TimeSpan
milliseconds i = TimeSpan (fromIntegral i * 1000)

milliseconds' :: Integer -> TimeSpan
milliseconds' i = TimeSpan (i * 1000)

seconds :: Int -> TimeSpan
seconds i = TimeSpan (fromIntegral i * 1000000)

seconds' :: Integer -> TimeSpan
seconds' i = TimeSpan (i * 1000000)

doubleSeconds :: Double -> TimeSpan
doubleSeconds d = TimeSpan (round $ d * 1000000)

minutes :: Int -> TimeSpan
minutes i = TimeSpan (fromIntegral i * 1000000 * 60)

hours :: Int -> TimeSpan
hours i = TimeSpan (fromIntegral i * 1000000 * 60 * 60)

days :: Int -> TimeSpan
days i = TimeSpan (fromIntegral i * 24 * 1000000 * 60 * 60)

years :: Int -> TimeSpan
years i = days (i * 365)

asNominalDiffTime :: TimeSpan -> NominalDiffTime
asNominalDiffTime = asSeconds'

asMicroseconds :: Integral i => TimeSpan -> i
asMicroseconds = fromIntegral . unTimeSpan

asMilliseconds :: Integral i => TimeSpan -> i
asMilliseconds (TimeSpan t) = fromIntegral (t `div` 1000)

asSeconds :: Integral i => TimeSpan -> i
asSeconds (TimeSpan t) = fromIntegral (t `div` 1000000)

asSeconds' :: Fractional a => TimeSpan -> a
asSeconds' (TimeSpan t) = fromInteger t / (1000 * 1000)

asMinutes :: Integral i => TimeSpan -> i
asMinutes (TimeSpan t) = fromIntegral (t `div` (1000000 * 60))

asHours :: Integral i => TimeSpan -> i
asHours (TimeSpan t) = fromIntegral (t `div` (1000000 * 60 * 60))

asDays :: TimeSpan -> Double
asDays (TimeSpan t) = (fromIntegral t) / (1000000 * 60 * 60 * 24)

diffTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
diffTimeSpan (TimeSpan t1) (TimeSpan t0) = TimeSpan (t1 - t0)
