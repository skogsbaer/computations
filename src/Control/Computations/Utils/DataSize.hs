{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.DataSize (
  DataSize (..),
  toBytes,
  toMebi,
  toKibi,
  displayDataSize,
  bytes,
  kibibytes,
  mebibytes,
  kibibytes',
  mebibytes',
  gibibytes',
  addDataSize,
  divDataSize',
  multDataSize',
  sumDataSize,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Hashable (Hashable)
import Data.LargeHashable
import qualified Data.Text as T
import Data.Word (Word64)
import Test.Framework
import Text.Printf

newtype DataSize = DataSize
  {unDataSize :: Word64 {- bytes -}}
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Num, Hashable, LargeHashable)

sumDataSize :: [DataSize] -> DataSize
sumDataSize = DataSize . sum . map unDataSize

toBytes :: Integral i => DataSize -> i
toBytes = fromIntegral . unDataSize

toKibi :: Fractional a => DataSize -> a
toKibi = (/ 1024) . fromInteger . toBytes

toMebi :: Fractional a => DataSize -> a
toMebi = (/ 1024) . toKibi

bytes :: Int -> DataSize
bytes = DataSize . fromIntegral

kibibytes :: Int -> DataSize
kibibytes = DataSize . (* 1024) . fromIntegral

mebibytes :: Int -> DataSize
mebibytes = DataSize . (* (1024 * 1024)) . fromIntegral

kibibytes' :: Double -> DataSize
kibibytes' = DataSize . round . (* 1024)

mebibytes' :: Double -> DataSize
mebibytes' = DataSize . round . (* (1024 * 1024))

gibibytes' :: Double -> DataSize
gibibytes' = DataSize . round . (* (1024 * 1024 * 1024))

addDataSize :: DataSize -> DataSize -> DataSize
addDataSize x y =
  DataSize $ unDataSize x + unDataSize y

divDataSize' :: RealFrac f => DataSize -> f -> DataSize
divDataSize' ds x =
  DataSize $ round $ fromIntegral (unDataSize ds) / x

multDataSize' :: RealFrac f => f -> DataSize -> DataSize
multDataSize' x ds =
  DataSize $ round $ x * fromIntegral (unDataSize ds)

displayDataSize :: DataSize -> String
displayDataSize (DataSize (fromIntegral -> bytes))
  | bytes < 2048 = num bytes ++ "B"
  | kibis < 2048 = num kibis ++ "KiB"
  | mebis < 2048 = num mebis ++ "MiB"
  | otherwise = num gibis ++ "GiB"
 where
  printDouble :: String -> Double -> String
  printDouble = printf
  num :: Double -> String
  num x =
    case T.stripSuffix ".0" (T.pack (printf "%f" x)) of
      Nothing -> printDouble "%.1f" $ (/ 10) $ fromIntegral @Int $ round $ x * 10
      Just x -> T.unpack x
  kibis :: Double
  kibis = bytes / 1024
  mebis :: Double
  mebis = kibis / 1024
  gibis :: Double
  gibis = mebis / 1024

test_displayDataSize :: IO ()
test_displayDataSize =
  do
    assertEqual "1024B" (displayDataSize $ bytes 1024)
    assertEqual "2047B" (displayDataSize $ bytes 2047)
    assertEqual "2KiB" (displayDataSize $ bytes 2048)
    assertEqual "56.8KiB" (displayDataSize $ bytes 58182)
    assertEqual "58.6KiB" (displayDataSize $ bytes 59960)

instance Arbitrary DataSize where
  arbitrary = DataSize <$> arbitrary
