{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.IncComps.CompEngine.Core (
  -- * Garbage, i.e. output produced by compuation applications that are

  -- no longer alive.
  Garbage (..),
  emptyGarbage,
  isGarbageEmpty,

  -- * Abstract interface to the state of an implementation of the engine

  -- running computations.
  CompEngineStateIf (..),
  CapEvaluationFinished,
  CapLookup (..),
  CapCached (..),
  CapResult (..),
  EnqueueInfo (..),
  RecompInfo (..),
  fromCapResult,
  optionToCapResult,
  capResultToVer,
  capCachedHash,
  mkRecompInfoMap,
  logStale,
  dependOn,
  tellDep,

  -- * Abstract interface to the computation engine
  CompEngineIfs (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.CompFlowRegistry
import Control.IncComps.CompEngine.CompSink
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.CompEngine.Types
import Control.IncComps.Utils.Hash
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.MultiMap as MM
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad.Reader
import Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable

data Garbage = Garbage
  { garbage_caps :: HashSet AnyCompAp
  -- ^ Caps that were garbage collected
  , garbage_deps :: HashSet AnyCompSrcKey
  -- ^ Dependencies that are no longer referenced
  , garbage_outputs :: HashMap AnyCompAp AnyCompSinkOutsMap
  -- ^ Outputs that are no longer generated
  -- indexed by the computation that created them
  }
  deriving (Eq, Show)

instance Semigroup Garbage where
  (<>) (Garbage caps1 deps1 outs1) (Garbage caps2 deps2 outs2) =
    Garbage
      (caps1 <> caps2)
      (deps1 <> deps2)
      (HashMap.unionWith unionAnyCompSinkOutsMap outs1 outs2)

instance Monoid Garbage where
  mempty = Garbage HashSet.empty HashSet.empty mempty

emptyGarbage :: Garbage
emptyGarbage = mempty

isGarbageEmpty :: Garbage -> Bool
isGarbageEmpty (Garbage caps deps outs) =
  HashSet.null caps && HashSet.null deps && HashMap.null outs

data CompEngineStateIf m = CompEngineStateIf
  { lookupCapResult
      :: forall a
       . CompAp a
      -> m (CapLookup (CapResult (CapCached a)))
  , capEvaluationStarted :: forall a. CompAp a -> m ()
  , capEvaluationFinished :: forall a. CapEvaluationFinished m a
  , dequeueGivenCap :: forall a. CompAp a -> m Bool
  , dequeueNextCap :: m (Maybe AnyCompAp)
  , staleQueueSize :: m Int
  , enqueueStaleCaps :: forall t. Foldable t => t CompEngDep -> m EnqueueInfo
  , trackOutput :: forall a. CompAp a -> AnyCompSinkOutsMap -> m ()
  , getCompSinkOuts :: forall s. CompSink s => s -> m (CompSinkOuts s)
  , getQueue :: m [AnyCompAp]
  -- ^ view of the queue, just for tests
  }

type CapEvaluationFinished m a =
  CompAp a
  -> DepSet
  -> Maybe a
  -> m
      ( HashSet AnyCompAp -- stale caps (only for logging purposes)
      , Garbage -- caps and tracked outputs that were garbage collected
      )

data CapLookup a
  = CapNotFound
  | CapFound a
  deriving (Show, Eq, Functor)

data CapResult a
  = CapSuccess a
  | CapFailure
  deriving (Show, Eq, Functor, Typeable)

fromCapResult :: a -> CapResult a -> a
fromCapResult def CapFailure = def
fromCapResult _ (CapSuccess x) = x

optionToCapResult :: Option a -> CapResult a
optionToCapResult None = CapFailure
optionToCapResult (Some x) = CapSuccess x

data CapCached a
  = CapMetaCached CompCacheMeta
  | CapValueCached (CompApResult a)
  deriving (Show, Eq)

capResultToVer :: CapResult (CapCached a) -> CompDepVer
capResultToVer r =
  CompDepVer $
    case r of
      CapFailure -> None
      CapSuccess x -> Some (capCachedHash x)

capCachedHash :: CapCached a -> Hash128
capCachedHash cc =
  case cc of
    CapMetaCached ccm -> ccm_largeHash ccm
    CapValueCached cv -> ccv_largeHash (cr_cacheValue cv)

data EnqueueInfo = EnqueueInfo
  { ei_affectedCaps :: Map AnyCompAp RecompInfo
  , ei_currentQueueSize :: Int
  }
  deriving (Show, Eq)

newtype RecompInfo = RecompInfo
  { ri_recompTrigger :: HashSet CompEngDep
  }
  deriving (Show, Eq)

mkRecompInfoMap
  :: (Foldable t)
  => [(CompEngDep, t AnyCompAp)]
  -> (Map AnyCompAp RecompInfo)
mkRecompInfoMap depsWithStaleCaps =
  F.foldl' insertForDep Map.empty depsWithStaleCaps
 where
  insertForDep m (dep, caps) =
    F.foldl' (insertForCap dep) m caps
  insertForCap dep m cap =
    let f _ (RecompInfo old) = RecompInfo (HashSet.insert dep old)
     in Map.insertWith f cap (RecompInfo (HashSet.singleton dep)) m

logStale :: (Foldable t, MonadIO m) => String -> t AnyCompAp -> m ()
logStale changerepr foldable =
  case caps of
    [] -> logDebug (changerepr ++ " didn't lead to stale caps.")
    [key] -> logInfo (changerepr ++ " invalidates " ++ show key)
    _
      | count < 10 ->
          do
            logInfo (changerepr ++ " invalidates the following caps:")
            mapM_ (logInfo . (" " ++) . show) caps
      | MM.numberOfKeys capsByType < 20 ->
          do
            logInfo $
              changerepr
                ++ " invalidates "
                ++ show count
                ++ " instances "
                ++ "of the following comps:"
            forM_ (MM.toSetList capsByType) $ \(ty, capsOfTy@(HashSet.size -> capsOfTyCount)) ->
              if capsOfTyCount < 5
                then mapM_ (logInfo . (" " ++) . show) capsOfTy
                else do
                  logInfo $
                    show capsOfTyCount
                      ++ " caps of type "
                      ++ show ty
                      ++ ". "
                      ++ "Here are 3 of them:"
                  mapM_ (logInfo . ("  " ++) . show) (take 3 (HashSet.toList capsOfTy))
      | otherwise ->
          do
            logInfo ("Oh! " ++ changerepr ++ " led to " ++ show count ++ " stale caps!")
            mapM_ (\k -> logDebug (show k)) caps
 where
  caps = map anyCapId $ F.toList foldable
  count = length caps
  capsByType = MM.fromList [(capId_compId x, x) | x <- caps]

dependOn :: AnyCompSrcDep -> CompM ()
dependOn = tellDep . HashSet.singleton . CompEngDepSrc

tellDep :: DepSet -> CompM ()
tellDep dep =
  CompM $ \_r -> (dep, CompFinished (CompResultOk ()))

data CompEngineIfs = CompEngineIfs
  { ce_compFlowRegistry :: CompFlowRegistry
  , ce_stateIf :: CompEngineStateIf IO
  }
