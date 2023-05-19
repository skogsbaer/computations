{-# OPTIONS_GHC -F -pgmF htfpp #-}

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
  plusTimeSpan,
  multiplyTimeSpan,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Utils.Parser
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Hashable
import Data.LargeHashable
import Data.Time.Clock
import GHC.Generics (Generic)
import Test.Framework
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- | Represents a time span with microsecond resolution.
newtype TimeSpan = TimeSpan {unTimeSpan :: Integer}
  deriving newtype (Eq, Ord, Num, Hashable)
  deriving stock (Generic)

instance LargeHashable TimeSpan

instance Arbitrary TimeSpan where
  arbitrary =
    do
      us <- arbitrary
      s <- arbitrary
      h <- arbitrary
      d <- arbitrary
      return (microseconds us + seconds s + hours h + days d)

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

test_showTimeSpan :: IO ()
test_showTimeSpan =
  do
    assertEqual "1ms1us" (show $ TimeSpan 1001)
    assertEqual "1days1us" (show $ days 1 `plusTimeSpan` microseconds 1)

timeSpanP :: Parser TimeSpan
timeSpanP =
  do
    spaceP
    firstTime <- join $ parseAsMs <$> integerP <*> P.many P.letterChar
    otherTimes <-
      do
        otherUs <- P.many $ (,) <$> L.decimal <*> P.many P.letterChar
        sum <$> mapM (Prelude.uncurry parseAsMs) otherUs
    return . TimeSpan $ firstTime `with` otherTimes
 where
  with initTime rest =
    let f = if initTime < 0 then (-) else (+)
     in initTime `f` rest
  parseAsMs :: Integer -> String -> Parser Integer
  parseAsMs num = \case
    "us" -> return num
    "ms" -> return $ 1000 * num
    "s" -> return $ 1000 ^ two * num
    "m" -> return $ toMin num
    "min" -> return $ toMin num
    "h" -> return $ 1000 ^ two * 60 ^ two * num
    "d" -> return $ toDays num
    "days" -> return $ toDays num
    u ->
      fail ("Got number " ++ show num ++ " with invalid unit: " ++ u)
  toMin num = 1000 ^ two * 60 * num
  toDays num = 1000 ^ two * 60 ^ two * 24 * num
  two = 2 :: Integer

test_timeSpanP :: IO ()
test_timeSpanP =
  do
    assertEqual (Ok $ minutes 3) (parseM timeSpanP "" "3m")
    assertEqual (Ok $ minutes 3) (parseM timeSpanP "" "3min")
    assertEqual (Ok $ days (-1)) (parseM timeSpanP "" "-1days")
    assertEqual (Ok $ days (-1)) (parseM timeSpanP "" "-1d")
    assertEqual (Ok (days (-1), " ")) (parseM' timeSpanP "" " -1d ")
    assertEqual (Ok $ days (-1) `plusTimeSpan` hours (-1)) (parseM timeSpanP "" "-1days1h")
    assertEqual (Ok $ days 0) (parseM timeSpanP "" " 0us")
    assertEqual (Ok longTime) (parseM timeSpanP "" "3days3h2s999ms998us")
    assertBool $ isFail (parseM timeSpanP "" "-1days-1h")
    assertBool $ isFail (parseM timeSpanP "" "-1days 1h")
 where
  longTime =
    foldr
      plusTimeSpan
      (days 0)
      [days 3, hours 3, seconds 2, milliseconds 999, microseconds 998]

prop_timeSpanP :: TimeSpan -> Bool
prop_timeSpanP f =
  parseM timeSpanP "" (showText f) == Ok f

instance Read TimeSpan where
  readsPrec _ = parserToReadsPrec timeSpanP

prop_readShow :: TimeSpan -> Bool
prop_readShow ts = ts == read (show ts)

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

plusTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
plusTimeSpan (TimeSpan t1) (TimeSpan t0) = TimeSpan (t1 + t0)

multiplyTimeSpan :: Real a => a -> TimeSpan -> TimeSpan
multiplyTimeSpan x (TimeSpan t) = TimeSpan . round $ toRational x * toRational t
