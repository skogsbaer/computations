{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.IncComps.CompEngine.CompDef (
  CompDef (..),
  mkComp,
  mkCompX,
  mkCompWithPriority,
  mkIncComp,
  CompDefM,
  runCompDefM,
  defineComp,
  defineRecursiveComp,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.Types
import Control.IncComps.CompEngine.CacheBehaviors
import Control.IncComps.CompEngine.Utils.PriorityAgingQueue (PaqPriority (..))
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.LargeHashable
import Data.Typeable
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Maybe

newtype CompDef p a = CompDef {unCompDef :: CompMap -> Comp p a}

mkCompX
  :: forall p r
   . String
  -> CompCacheBehavior r
  -> CompFunX p r
  -> CompDef p r
mkCompX name caching fun =
  CompDef $ \cm -> Comp (mkCompId name) caching fun cm

mkComp
  :: String
  -> CompCacheBehavior r
  -> CompFun p r
  -> CompDef p r
mkComp a c f = mkCompX a c (f . ce_param)

mkCompWithPriority
  :: PaqPriority
  -> T.Text
  -> CompCacheBehavior r
  -> CompFun p r
  -> CompDef p r
mkCompWithPriority prio name caching fun =
  CompDef $ \cm -> Comp (mkCompIdWithPriority prio name) caching (fun . ce_param) cm

mkIncComp :: (LargeHashable r, Typeable r, Show r) => String -> r -> (p -> r -> CompM r) -> CompDef p r
mkIncComp name initState updateFun =
  mkCompX name inMemoryLHCaching $ \ce ->
 do mCachedValue <- ce_cachedResult ce
    updateFun (ce_param ce) (fromMaybe initState mCachedValue)

newtype CompDefM a = CompDefM (StateT CompMap Fail a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadFix)

getCompMap :: CompDefM CompMap
getCompMap = CompDefM get

defineComp :: (IsCompResult a, IsCompParam p) => CompDef p a -> CompDefM (Comp p a)
defineComp (CompDef mkComp) =
  do
    comp <- liftM mkComp getCompMap
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
