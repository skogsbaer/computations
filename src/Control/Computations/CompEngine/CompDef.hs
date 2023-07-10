{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Computations.CompEngine.CompDef (
  CompDef (..),
  defineComp,
  defineCompX,
  defineCompWithPriority,
  defineIncComp,
  CompWireM,
  runCompWireM,
  wireComp,
  defineRecursiveComp,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.Types
import Control.Computations.CompEngine.Utils.PriorityAgingQueue (PaqPriority (..))
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.LargeHashable
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T

newtype CompDef p a = CompDef {unCompDef :: CompMap -> Comp p a}

defineCompX
  :: forall p r .
     (IsCompParam p, IsCompResult r)
  => String
  -> CompCacheBehavior r
  -> CompFunX p r
  -> CompDef p r
defineCompX name caching fun =
  CompDef $ \cm -> Comp (mkCompId name) caching fun cm

defineComp
  :: (IsCompParam p, IsCompResult r)
  => String
  -> CompCacheBehavior r
  -> CompFun p r
  -> CompDef p r
defineComp a c f = defineCompX a c (f . ce_param)

defineCompWithPriority
  :: (IsCompParam p, IsCompResult r)
  => PaqPriority
  -> T.Text
  -> CompCacheBehavior r
  -> CompFun p r
  -> CompDef p r
defineCompWithPriority prio name caching fun =
  CompDef $ \cm -> Comp (mkCompIdWithPriority prio name) caching (fun . ce_param) cm

defineIncComp :: (IsCompParam p, IsCompResult r, LargeHashable r) => String -> r -> (p -> r -> CompM r) -> CompDef p r
defineIncComp name initState updateFun =
  defineCompX name fullCaching $ \ce ->
    do
      mCachedValue <- ce_cachedResult ce
      updateFun (ce_param ce) (fromMaybe initState mCachedValue)

newtype CompWireM a = CompWireM (StateT CompMap Fail a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadFix)

getCompMap :: CompWireM CompMap
getCompMap = CompWireM get

wireComp :: (IsCompResult a, IsCompParam p) => CompDef p a -> CompWireM (Comp p a)
wireComp (CompDef defineComp) =
  do
    comp <- liftM defineComp getCompMap
    insertComp comp

defineRecursiveComp
  :: (IsCompResult a, IsCompParam p)
  => (Comp p a -> CompDef p a)
  -> CompWireM (Comp p a)
defineRecursiveComp f =
  mfix $ \comp -> wireComp (f comp)

insertComp
  :: forall p a
   . (IsCompResult a, IsCompParam p)
  => Comp p a
  -> CompWireM (Comp p a)
insertComp comp =
  CompWireM $
    do
      seen <- get
      case Map.lookup (comp_name comp) seen of
        Just _ ->
          fail ("Computation " ++ show (comp_name comp) ++ " already defined.")
        Nothing ->
          do
            put (Map.insert (comp_name comp) (AnyComp comp) seen)
            return comp

runCompWireM :: CompWireM a -> Fail (CompMap, a)
runCompWireM (CompWireM action) =
  fmap (\(x, y) -> (y, x)) (runStateT action Map.empty)
