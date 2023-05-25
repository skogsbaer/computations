{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Computations.CompEngine.CompEval (evalComp, evalCompOrFail, evalCompWithDefault, mkCompDepForCap) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.SourceLocation
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import qualified Data.HashSet as HashSet

evalComp
  :: (IsCompParam p, IsCompResult a)
  => Comp p a
  -> p
  -> CompM (Maybe a)
evalComp comp p =
  do
    mB <- doAnyRequest $ CompReqEval compAp
    tellDep (HashSet.singleton (mkCompDepForCap' compAp mB))
    return (fmap cr_returnValue mB)
 where
  compAp = mkCompAp comp p

evalCompOrFail
  :: (HasCallStack, IsCompParam p, IsCompResult a)
  => Comp p a
  -> p
  -> CompM a
evalCompOrFail comp p = do
  res <- evalComp comp p
  case res of
    Just x -> pure x
    Nothing -> do
      let msg =
            callerLocation
              ++ ": evaluation of "
              ++ show (comp_name comp)
              ++ " failed for argument "
              ++ show p
      fail msg

evalCompWithDefault
  :: (IsCompParam p, IsCompResult a)
  => Comp p a
  -> p
  -> a
  -> CompM a
evalCompWithDefault comp p def =
  do
    res <- evalComp comp p
    case res of
      Nothing -> pure def
      Just x -> pure x

mkCompDepForCap :: IsCompResult a => CompAp a -> Maybe a -> CompEngDep
mkCompDepForCap = mkCompDepForCapHelper mkCompDepVer

mkCompDepForCap' :: IsCompResult a => CompAp a -> Maybe (CompApResult a) -> CompEngDep
mkCompDepForCap' = mkCompDepForCapHelper mkCompDepVer'

mkCompDepForCapHelper
  :: IsCompResult r
  => (CompAp r -> t -> CompDepVer)
  -> CompAp r
  -> t
  -> CompEngDep
mkCompDepForCapHelper mkVer cap ma = mkCompDep (CompDepKey (AnyCompAp cap)) (mkVer cap ma)

mkCompDepVer :: CompAp a -> Maybe a -> CompDepVer
mkCompDepVer (CompAp{cap_comp = (Comp{comp_caching = ccb})}) =
  mkCompDepVerHelper (ccb_memcache ccb)

mkCompDepVer' :: CompAp a -> Maybe (CompApResult a) -> CompDepVer
mkCompDepVer' _cap = mkCompDepVerHelper cr_cacheValue

mkCompDepVerHelper :: (a -> CompCacheValue a1) -> Maybe a -> CompDepVer
mkCompDepVerHelper f = CompDepVer . fmap (ccv_largeHash . f) . maybeToOption
