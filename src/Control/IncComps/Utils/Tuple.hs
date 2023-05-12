{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.IncComps.Utils.Tuple (
  Pair (..),
  (:!:),
  toLazyTuple,
  fromLazyTuple,
  fst',
  snd',
  curry',
  uncurry',
  first',
  second',
  swap',
)
where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Data
import Data.Strict.Tuple hiding (fst, snd, zip)
import qualified Data.Strict.Tuple as S
import Test.QuickCheck

deriving instance Typeable Pair

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = (:!:) <$> arbitrary <*> arbitrary

toLazyTuple :: a :!: b -> (a, b)
toLazyTuple (x :!: y) = (x, y)

fromLazyTuple :: (a, b) -> a :!: b
fromLazyTuple (x, y) = (x :!: y)

fst' :: Pair a b -> a
fst' = S.fst

snd' :: Pair a b -> b
snd' = S.snd

curry' :: (Pair a b -> c) -> a -> b -> c
curry' = S.curry

uncurry' :: (a -> b -> c) -> Pair a b -> c
uncurry' = S.uncurry

first' :: (a -> b) -> (a :!: c) -> (b :!: c)
first' f (a :!: c) = (f a :!: c)

second' :: (b -> c) -> (a :!: b) -> (a :!: c)
second' f (a :!: b) = (a :!: f b)

swap' :: (a :!: b) -> (b :!: a)
swap' = S.swap
