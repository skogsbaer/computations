module Control.IncComps.Utils.StrictList (
  module SL,
  SL,
  null,
  singleton,
  merge,
  mergeBy,
  toList,
  fromList,
) where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------

import GHC.Exts
import StrictList as SL

type SL a = SL.List a

singleton :: a -> SL a
singleton x = SL.Cons x SL.Nil

merge :: Ord a => SL a -> SL a -> SL a
merge = mergeBy compare

mergeBy :: (a -> a -> Ordering) -> SL a -> SL a -> SL a
mergeBy cmp = go
 where
  go as@(SL.Cons a as') bs@(SL.Cons b bs') =
    case cmp a b of
      LT -> SL.Cons a (go as' bs)
      GT -> SL.Cons b (go as bs')
      EQ -> SL.Cons a (go as' bs')
  go Nil bs = bs
  go as Nil = as
