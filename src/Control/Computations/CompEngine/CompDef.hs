{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Computations.CompEngine.CompDef (
  CompDef (..),
  mkCompDef,
  mkCompDefX,
  mkCompDefWithPriority,
  mkIncCompDef,
  CompDefM,
  runCompDefM,
  defineComp,
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

mkCompDefX
  :: forall p r
   . String
  -> CompCacheBehavior r
  -> CompFunX p r
  -> CompDef p r
mkCompDefX name caching fun =
  CompDef $ \cm -> Comp (mkCompId name) caching fun cm

mkCompDef
  :: String
  -> CompCacheBehavior r
  -> CompFun p r
  -> CompDef p r
mkCompDef a c f = mkCompDefX a c (f . ce_param)

mkCompDefWithPriority
  :: PaqPriority
  -> T.Text
  -> CompCacheBehavior r
  -> CompFun p r
  -> CompDef p r
mkCompDefWithPriority prio name caching fun =
  CompDef $ \cm -> Comp (mkCompIdWithPriority prio name) caching (fun . ce_param) cm

mkIncCompDef :: (IsCompResult r, LargeHashable r) => String -> r -> (p -> r -> CompM r) -> CompDef p r
mkIncCompDef name initState updateFun =
  mkCompDefX name fullCaching $ \ce ->
    do
      mCachedValue <- ce_cachedResult ce
      updateFun (ce_param ce) (fromMaybe initState mCachedValue)

newtype CompDefM a = CompDefM (StateT CompMap Fail a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadFix)

getCompMap :: CompDefM CompMap
getCompMap = CompDefM get

defineComp :: (IsCompResult a, IsCompParam p) => CompDef p a -> CompDefM (Comp p a)
defineComp (CompDef mkCompDef) =
  do
    comp <- liftM mkCompDef getCompMap
    insertComp comp

defineRecursiveComp
  :: (IsCompResult a, IsCompParam p)
  => (Comp p a -> CompDef p a)
  -> CompDefM (Comp p a)
defineRecursiveComp f =
  mfix $ \comp -> defineComp (f comp)

insertComp
  :: forall p a
   . (IsCompResult a, IsCompParam p)
  => Comp p a
  -> CompDefM (Comp p a)
insertComp comp =
  CompDefM $
    do
      seen <- get
      case Map.lookup (comp_name comp) seen of
        Just _ ->
          fail ("Computation " ++ show (comp_name comp) ++ " already defined.")
        Nothing ->
          do
            put (Map.insert (comp_name comp) (AnyComp comp) seen)
            return comp

runCompDefM :: CompDefM a -> Fail (CompMap, a)
runCompDefM (CompDefM action) =
  fmap (\(x, y) -> (y, x)) (runStateT action Map.empty)
