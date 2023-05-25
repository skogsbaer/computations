{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Computations.CompEngine.SimpleStateIf (
  SimpleStateIf (..),
  SifState (..),
  mkSimpleCompEngineStateIf,
  initialSifState,
  validateSifState,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.SifCache (SifCache)
import qualified Control.Computations.CompEngine.SifCache as SifCache
import Control.Computations.CompEngine.Types
import Control.Computations.CompEngine.Utils.DepMap
import qualified Control.Computations.CompEngine.Utils.DepMap as DepMap
import qualified Control.Computations.CompEngine.Utils.OutputsMap as OM
import qualified Control.Computations.CompEngine.Utils.PriorityAgingQueue as Paq
import Control.Computations.Utils.Logging
import qualified Control.Computations.Utils.StrictList as SL
import Control.Computations.Utils.Tuple
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import Data.Foldable (foldr')
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Prim

newtype SimpleStateIf m = SimpleStateIf
  { ssif_withState :: forall a. (SifState -> (a, SifState)) -> m a
  }

type SifDeps = DepMap AnyCompAp CompEngDep
type SifVerMap = Map AnyCompAp CompDepVer
type SifOutputs = OM.OutputsMap AnyCompAp
type SifPendingOutputs = Map AnyCompAp AnyCompSinkOutsMap
type SifStale = Paq.PriorityAgingQueue AnyCompAp ()
type SifPendingCaps = Set AnyCompAp

data SifState = SifState
  { sifs_deps :: SifDeps
  , sifs_cache :: SifCache
  , sifs_vermap :: SifVerMap
  , sifs_outputs :: SifOutputs
  , sifs_pendingOutputs :: SifPendingOutputs
  , sifs_pendingCaps :: SifPendingCaps
  -- ^ caps for which calculation has been started but not finished
  , sifs_stale :: SifStale
  }
  deriving (Show)

-- | Checks invariants about the SifState
validateSifState :: SifState -> IO ()
validateSifState state =
  do
    let keysFromDeps = Set.fromList . F.toList . DepMap.keys $ sifs_deps state
        deps = DepMap.depKeys (sifs_deps state)
        deps' =
          flip mapMaybe (F.toList deps) $ \case
            CompEngDepKeySrc _ -> Nothing
            CompEngDepKeyComp depKey -> Just (unCompDepKey depKey)
        rejectIfNotIdentical :: (Ord a, Show a) => String -> Set a -> a -> IO (Set a)
        rejectIfNotIdentical what xs !x =
          -- FIXME
          case Set.lookupLE x xs of
            Just !y | y == x ->
              case reallyUnsafePtrEquality# x y of
                0# ->
                  fail $
                    "When validating "
                      ++ what
                      ++ ": got 2 different thunks for "
                      ++ show x
                      ++ "\nSet: "
                      ++ show xs
                _ -> return xs
            _ -> return (Set.insert x xs)
    validatedKeysFromDeps <-
      foldM (rejectIfNotIdentical "deps'") keysFromDeps deps'
    let keysFromCache = map fst $ SifCache.toList (sifs_cache state)
    validatedKeysAfterCache <-
      foldM (rejectIfNotIdentical "keysFromCache") validatedKeysFromDeps keysFromCache
    let keysFromVerMap = map fst $ Map.toList (sifs_vermap state)
    validatedKeysAfterVerMap <-
      foldM (rejectIfNotIdentical "keysFromVerMap") validatedKeysAfterCache keysFromVerMap
    let keysFromOutputsMap = map fst . HashMap.toList . OM.forwardMap $ sifs_outputs state
    _validatedKeysAfterOutputs <-
      foldM (rejectIfNotIdentical "keysFromOutputsMap") validatedKeysAfterVerMap keysFromOutputsMap
    return ()

initialSifState :: SifState
initialSifState =
  SifState
    { sifs_deps = DepMap.empty
    , sifs_cache = emptyCache
    , sifs_vermap = mempty
    , sifs_outputs = OM.empty
    , sifs_pendingOutputs = mempty
    , sifs_pendingCaps = mempty
    , sifs_stale = Paq.empty
    }

emptyCache :: SifCache
emptyCache = SifCache.empty

mkSimpleCompEngineStateIf
  :: (Monad m)
  => SimpleStateIf m
  -> CompEngineStateIf m
mkSimpleCompEngineStateIf sif =
  CompEngineStateIf
    { lookupCapResult = lookupCapResultImpl sif
    , capEvaluationStarted = capEvaluationStartedImpl sif
    , capEvaluationFinished = putCapResultImpl sif
    , dequeueGivenCap = dequeueGivenCapImpl sif
    , staleQueueSize = staleQueueSizeImpl sif
    , dequeueNextCap = dequeueNextCapImpl sif
    , enqueueStaleCaps = enqueueStaleCapsImpl sif
    , trackOutput = trackOutputImpl sif
    , getCompSinkOuts = getDataIfOutputsImpl sif
    , getQueue = getQueueImpl sif
    }

getQueueImpl :: Monad m => SimpleStateIf m -> m [AnyCompAp]
getQueueImpl sif =
  do
    (Paq.PaqView rtq xq rq bq) <- fromSifState sif (Paq.view . sifs_stale)
    return $! map Paq.paqe_key $ concat [rtq, xq, rq, bq]

staleQueueSizeImpl :: SimpleStateIf m -> m Int
staleQueueSizeImpl sif =
  fromSifState sif $ \st -> Paq.size (sifs_stale st)

trackOutputImpl
  :: (Monad m, IsCompResult a)
  => SimpleStateIf m
  -> CompAp a
  -> AnyCompSinkOutsMap
  -> m ()
trackOutputImpl sif cap outputs
  | nullAnyOutsMap outputs = return ()
  | otherwise =
      withSifState sif $ \st ->
        let outputsStr = unlines (map ("  - " ++) (lines (show outputs)))
            msg = (show (capId cap) ++ " produced the following outputs:\n" ++ outputsStr)
            newOutputs = Map.insertWith mappend (AnyCompAp cap) outputs (sifs_pendingOutputs st)
            newSt = st{sifs_pendingOutputs = newOutputs}
         in pureDebug msg ((), newSt)

commitPendingOutputsForKey :: SifState -> AnyCompAp -> SifState
commitPendingOutputsForKey st key =
  case Map.lookup key (sifs_pendingOutputs st) of
    Nothing -> st{sifs_outputs = OM.insert key mempty (sifs_outputs st)}
    Just anyOuts ->
      let warnMsgs =
            catMaybes $
              do
                ForAnyCompFlow ident p (SomeCompSinkOuts outs) <-
                  Map.elems (unAnyCompSinkOutsMap anyOuts)
                output <- F.toList outs
                let revLookupResult =
                      OM.lookupOutputKey
                        (p, ident, output)
                        (sifs_outputs st)
                case SL.filter (/= key) revLookupResult of
                  SL.Nil -> pure Nothing
                  otherCapKeys ->
                    pure $
                      Just
                        ( "MULTIPLE_COMPAPS_ONE_OUTPUT: The output "
                            ++ show output
                            ++ " has just been generated by the comp ap "
                            ++ showAnyCompApDetails key
                            ++ ". Before, this output has been generated by the comp ap(s) "
                            ++ intercalate ", " (map showAnyCompApDetails (SL.toList otherCapKeys))
                            ++ ". "
                            ++ " This 'travelling' of outputs of one comp ap "
                            ++ "to another is outlawed since it can lead to outdated "
                            ++ "outputs. FIX THIS!"
                        )
          doWarnings =
            if F.null warnMsgs
              then id
              else pureWarn (unlines warnMsgs)
       in doWarnings $
            st
              { sifs_outputs = OM.insert key anyOuts (sifs_outputs st)
              , sifs_pendingOutputs = Map.delete key (sifs_pendingOutputs st)
              }

getDataIfOutputsImpl
  :: forall s m
   . (CompSink s)
  => SimpleStateIf m
  -> s
  -> m (CompSinkOuts s)
getDataIfOutputsImpl sif s =
  fromSifState sif $ \st ->
    foldr'
      (\out set -> unionOuts (anyOutputsToCompSinkOutputs out) set)
      HashSet.empty
      (HashMap.elems $ OM.forwardMap $ sifs_outputs st)
 where
  anyOutputsToCompSinkOutputs :: AnyCompSinkOutsMap -> Maybe (CompSinkOuts s)
  anyOutputsToCompSinkOutputs m = compSinkOutsFromAny (Proxy @s) (compSinkId s) m
  unionOuts Nothing set = set
  unionOuts (Just set1) set2 = set1 `HashSet.union` set2

lookupCapResultImpl
  :: IsCompResult a
  => SimpleStateIf m
  -> CompAp a
  -> m (CapLookup (CapResult (CapCached a)))
lookupCapResultImpl sif cap =
  withSifState sif $ \sifState ->
    let (_, res, sifState') = lookupCapResultImpl' cap sifState
     in (res, sifState')

lookupCapResultImpl'
  :: IsCompResult r
  => CompAp r
  -> SifState
  -> (AnyCompAp, CapLookup (CapResult (CapCached r)), SifState)
lookupCapResultImpl' cap st =
  let keyForCap = AnyCompAp cap
      res = SifCache.lookup keyForCap (sifs_cache st)
      (keyFromCache, result, sifs_cache') =
        case res of
          Nothing -> (keyForCap, CapNotFound, sifs_cache st)
          Just (key, CapFailure) -> (key, CapFound CapFailure, sifs_cache st)
          Just (key, CapSuccess (AnyCompCacheValue ccvAnyType)) ->
            let optionCvc = fmap capCached (castCompCacheValue ccvAnyType)
             in (key, CapFound (optionToCapResult optionCvc), sifs_cache st)
      st' =
        st
          { sifs_cache = sifs_cache'
          }
   in (keyFromCache, result, st')
 where
  capCached ccv =
    case ccv_payload ccv of
      Some value -> CapValueCached (CompApResult value ccv)
      None -> CapMetaCached (ccv_meta ccv)

fromSifState :: SimpleStateIf m -> (SifState -> a) -> m a
fromSifState (SimpleStateIf{..}) f = ssif_withState (\x -> (f x, x))

withSifState :: SimpleStateIf m -> (SifState -> (a, SifState)) -> m a
withSifState (SimpleStateIf{..}) = ssif_withState

dequeueNextCapImpl :: Monad m => SimpleStateIf m -> m (Maybe (AnyCompAp))
dequeueNextCapImpl sif =
  do
    res <- withSifState sif nextStaleCap
    return res
 where
  nextStaleCap :: SifState -> (Maybe AnyCompAp, SifState)
  nextStaleCap ges =
    case Paq.dequeue curQ of
      None ->
        pureDebug
          ("Cache has " ++ show instanceCount ++ " entries.")
          (Nothing, ges)
      Some (e :!: newQ) ->
        pureDebug
          ( "Dequeued "
              ++ show (Paq.paqe_key e)
              ++ " with "
              ++ show (Paq.size newQ)
              ++ " stale caps remaining."
          )
          $ let newSt =
                  ges
                    { sifs_stale = newQ
                    }
             in (Just (Paq.paqe_key e), newSt)
   where
    curQ = sifs_stale ges
    cache = sifs_cache ges
    instanceCount = SifCache.totalInstanceCount cache

capEvaluationStartedImpl :: IsCompResult a => SimpleStateIf m -> CompAp a -> m ()
capEvaluationStartedImpl sif cap =
  withSifState sif $ \s ->
    let newPendingCaps = Set.insert (AnyCompAp cap) (sifs_pendingCaps s)
     in ((), s{sifs_pendingCaps = newPendingCaps})

putCapResultImpl
  :: SimpleStateIf m
  -> CompAp a
  -> DepSet
  -> Maybe a
  -> m (HashSet AnyCompAp, Garbage)
putCapResultImpl sif cap deps mres =
  withSifState sif (putCapRes cap deps (maybeToOption mres))

runGc
  :: String
  -> SifState
  -> Garbage
  -> HashSet (DepKey CompEngDep)
  -> (SifState, Garbage)
runGc capId !origS del@(Garbage _delCaps _delDeps delOutsMap) garbage =
  case HashSet.toList garbage of
    [] ->
      let delOuts = unionsAnyCompSinkOutsMap $ HashMap.elems delOutsMap
       in ( origS
          , if not (nullAnyOutsMap delOuts)
              then
                pureInfo
                  ( "Change of "
                      ++ capId
                      ++ " unreferenced "
                      ++ ( if sizeAnyOutsMap delOuts == 1
                            then "one output: "
                            else show (sizeAnyOutsMap delOuts) ++ " outputs:\n"
                         )
                      ++ ( intercalate "\n" $
                            map (("- " ++) . show) (anyOutsMapToList delOuts)
                         )
                  )
                  del
              else del
          )
    x@(CompEngDepKeyComp (CompDepKey cap)) : _ ->
      let (dm', garbage') = DepMap.delete cap (sifs_deps origS)
          oldOuts = sifs_outputs origS
          newOuts = OM.delete cap oldOuts
          newS =
            origS
              { sifs_deps = dm'
              , sifs_cache = SifCache.delete cap (sifs_cache origS)
              , sifs_vermap = Map.delete cap (sifs_vermap origS)
              , sifs_outputs = newOuts
              , sifs_stale = Paq.delete cap (sifs_stale origS)
              }
          newGarbage = HashSet.delete x garbage `HashSet.union` garbage'
          garbageOutputs =
            maybe mempty (OM.filterUnreferencedOutputs newOuts) $
              OM.lookup cap oldOuts
          moreDel =
            emptyGarbage
              { garbage_caps = HashSet.singleton cap
              , garbage_outputs =
                  if garbageOutputs == mempty
                    then mempty
                    else HashMap.singleton cap garbageOutputs
              }
          del' = del `mappend` moreDel
       in runGc capId newS del' newGarbage
    x@(CompEngDepKeySrc srcDepKey) : _ ->
      let moreDel =
            emptyGarbage{garbage_deps = HashSet.singleton srcDepKey}
       in runGc capId origS (del <> moreDel) (HashSet.delete x garbage)

insertCapWithDeps
  :: AnyCompAp
  -> HashSet CompEngDep
  -> SifState
  -> (SifState, Maybe (HashSet CompEngDep), Garbage)
insertCapWithDeps cap deps sIn =
  let capId = show (anyCapId cap)
      key = anyCapId cap
      mOldOutputs = OM.lookup cap (sifs_outputs sIn)
      oldOutputs = fromMaybe mempty mOldOutputs
      newOutputs = Map.findWithDefault mempty cap (sifs_pendingOutputs sIn)
      addOutputs = newOutputs `diffAnyOutsMap` oldOutputs
      delOutputs = oldOutputs `diffAnyOutsMap` newOutputs
      addCount = sizeAnyOutsMap addOutputs
      delCount = sizeAnyOutsMap delOutputs
      changeCount = addCount + delCount
      outputsChanged = newOutputs /= oldOutputs
      s = commitPendingOutputsForKey sIn cap
      garbOutputs = OM.filterUnreferencedOutputs (sifs_outputs s) delOutputs
      logOutputs x =
        logMsg1 msg1 $!
          logMsg2 msg2 $!
            x
       where
        logMsg1
          | changeCount > 0 && isJust mOldOutputs = pureInfo
          | changeCount > 0 = pureDebug
          | outputsChanged = pureDebug
          | nullAnyOutsMap oldOutputs && nullAnyOutsMap newOutputs = pureNoLog
          | otherwise = pureDebug
        logMsg2
          | changeCount > 0 = pureDebug
          | outputsChanged = pureDebug
          | otherwise = pureNoLog
        msg1 =
          ( capId
              ++ " now has "
              ++ show (sizeAnyOutsMap newOutputs)
              ++ " outputs. "
              ++ "It previously had "
              ++ show (sizeAnyOutsMap oldOutputs)
              ++ " (+"
              ++ show addCount
              ++ "/-"
              ++ show delCount
              ++ ")"
              ++ ( if changeCount == 0
                    then ". No keys changed."
                    else if changeCount > 1 then ":\n" else ": "
                 )
              ++ ( intercalate
                    "\n"
                    ( mapAnyOutsMap (\_ -> ("+ " ++) . show) addOutputs
                        ++ mapAnyOutsMap (\_ -> ("- " ++) . show) delOutputs
                    )
                 )
          )
        msg2 =
          ( "Outputs of "
              ++ show key
              ++ " changed:\n"
              ++ "Old outputs: "
              ++ show oldOutputs
              ++ "\n"
              ++ "New outputs: "
              ++ show newOutputs
          )
      (tempDepMap, mOldDeps, directGarbage) = DepMap.insert' cap deps (sifs_deps s)
      tempS = s{sifs_deps = tempDepMap}
      (newS, allGarbageCaps) = runGc capId tempS emptyGarbage directGarbage
      allGarbage =
        if nullAnyOutsMap garbOutputs
          then allGarbageCaps
          else
            allGarbageCaps
              `mappend` (mempty{garbage_outputs = HashMap.singleton cap garbOutputs})
      notify (x, d, g) =
        if g == mempty
          then (x, d, g)
          else
            pureDebug
              ( "Insertion of "
                  ++ capId
                  ++ " led to garbage: "
                  ++ show allGarbage
              )
              (x, d, g)
      msg = ("insertCapWithDeps " ++ capId ++ " <- " ++ show (HashSet.toList deps))
   in pureDebug msg $!
        logOutputs $!
          notify $!
            ( newS
            , mOldDeps
            , allGarbage
            )

putCapRes
  :: CompAp a
  -> DepSet
  -> Option a
  -> SifState
  -> ((HashSet AnyCompAp, Garbage), SifState)
putCapRes gap@(CompAp{cap_comp = Comp{comp_caching = CompCacheBehavior{..}}}) deps res s =
  let normalizeDep dep =
        case dep of
          CompEngDepSrc _ -> dep
          CompEngDepComp (CompDep (Dep (CompDepKey depKey) depVer)) ->
            case SifCache.lookup depKey (sifs_cache s) of
              Just (depKey', _) -> CompEngDepComp (CompDep (Dep (CompDepKey depKey') depVer))
              Nothing -> dep
      normalizedDeps = HashSet.map normalizeDep deps
      stale = DepMap.stale (mkCompDep (CompDepKey (AnyCompAp gap)) newVer) (sifs_deps s)
      ccv = fmap ccb_memcache res
      anyCcv = fmap AnyCompCacheValue ccv
      newVer = CompDepVer (fmap ccv_largeHash ccv)
      -- We want to keep the cap that is already stored in the cache to maximise sharing
      (capFromCache, oldCcv, s0) = lookupCapResultImpl' gap s
      key = anyCapId capFromCache
      oldVer = fmap capResultToVer oldCcv
      (s1, oldDeps, garbage) = insertCapWithDeps capFromCache normalizedDeps s0
      s2 =
        -- first delete current cap from queue - if it's stale it should be enqueued at the end
        s1{sifs_stale = Paq.delete capFromCache (sifs_stale s1)}
      s3 =
        let prevS = s2
            cache =
              SifCache.insert capFromCache (optionToCapResult anyCcv) (sifs_cache prevS)
            vermap = Map.insert capFromCache newVer (sifs_vermap prevS)
         in s2
              { sifs_cache = cache
              , sifs_vermap = vermap
              }
      (newStaleCaps, s4)
        | oldDeps == Just deps
            && oldVer /= CapNotFound
            && oldVer /= CapFound newVer =
            pureError
              ( "Impure cap "
                  ++ show gap
                  ++ " returned different result than previously "
                  ++ " although inputs didn't change.  Not marking anything as stale."
                  ++ " New result:\n"
                  ++ show ccv
                  ++ "\nOld result:\n"
                  ++ show oldCcv
                  ++ "\nDeps are: "
                  ++ concatMap (("\n - " ++) . show) deps
              )
              (HashSet.empty, s3)
        | otherwise =
            (if HashSet.null stale then pureNoLog else pureDebug)
              ( show key
                  ++ " invalidates "
                  ++ show (HashSet.size stale)
                  ++ ( if length stale >= 5
                        then " caps:" ++ concatMap (("\n " ++) . show) stale
                        else " caps " ++ show (HashSet.toList stale)
                     )
              )
              (invalidate stale s3)
      -- finally remove current cap from stack of pending caps (must be done after invalidate
      -- because invalidate only enqueues caps that are not currently being calculated)
      s5 =
        case sifs_pendingCaps s4 of
          xs
            | capFromCache `Set.member` xs ->
                s4{sifs_pendingCaps = Set.delete capFromCache xs}
            | otherwise ->
                pureError
                  ( "Tried to pop "
                      ++ show key
                      ++ " from pending set but "
                      ++ "it's not contained:\n"
                      ++ concatMap (("\n " ++) . show) xs
                  )
                  s4
      s6 =
        -- now store the current version of the cap in the verpmap and check if it's stale
        let changedDeps = mapMaybe checkIfDepChanged (HashSet.toList deps)
            checkIfDepChanged ceDep =
              case ceDep of
                CompEngDepSrc{} -> Nothing
                CompEngDepComp (CompDep d) ->
                  case Map.lookup (unCompDepKey (dep_key d)) (sifs_vermap s5) of
                    Nothing ->
                      pureError
                        ( "Cap "
                            ++ show key
                            ++ " depends on "
                            ++ show d
                            ++ " but "
                            ++ "it's not contained in the vermap."
                        )
                        Nothing
                    Just curver
                      | depVer d /= curver -> Just (d, curver)
                      | otherwise -> Nothing
            sNew =
              s5{sifs_stale = snd $ Paq.enqueue (mkPaqEntry capFromCache) (sifs_stale s5)}
         in case changedDeps of
              [] -> s5
              [(d, cur)] ->
                pureInfo
                  ( "Cap "
                      ++ show key
                      ++ " depends on the old "
                      ++ show d
                      ++ " but new version is "
                      ++ show cur
                      ++ ".  Invalidating current cap!"
                  )
                  sNew
              xs ->
                pureInfo
                  ( "Cap "
                      ++ show key
                      ++ " depends on old caps. Invalidating current cap!"
                      ++ concatMap (\(d, cur) -> "\n " ++ show d ++ " --> " ++ show cur) xs
                  )
                  sNew
   in ((newStaleCaps, garbage), s6)

dequeueGivenCapImpl :: IsCompResult a => SimpleStateIf m -> CompAp a -> m Bool
dequeueGivenCapImpl sif cap =
  withSifState sif $ \sifs0 ->
    let
      -- first remove the cap from the queue of stale caps
      (didDequeueCap, sifs1) =
        case Paq.deleteView (AnyCompAp cap) (sifs_stale sifs0) of
          Some (_ :!: q) ->
            pureDebug
              ( "Dequeued given cap "
                  ++ show key
                  ++ ", "
                  ++ show (Paq.size q)
                  ++ " stale caps remaining."
              )
              (True, sifs0{sifs_stale = q})
          None ->
            (False, sifs0)
     in
      (didDequeueCap, sifs1)
 where
  key = capId cap

mkPaqEntry :: AnyCompAp -> Paq.PaqEntry (AnyCompAp) ()
mkPaqEntry key =
  Paq.PaqEntry (Paq.PaqTime 0) (anyCompApPriority key) key ()

enqueueStaleCapsImpl
  :: (Foldable t)
  => SimpleStateIf m
  -> t (CompEngDep)
  -> m (EnqueueInfo)
enqueueStaleCapsImpl sif deps =
  withSifState sif $ \gesOld ->
    let (depsWithStaleCaps, gesNew) = notifyDepChanges deplist gesOld
        affectedStaleCaps = mkRecompInfoMap depsWithStaleCaps
        enqueueInfo =
          EnqueueInfo
            { ei_affectedCaps = affectedStaleCaps
            , ei_currentQueueSize = Paq.size (sifs_stale gesNew)
            }
     in (enqueueInfo, gesNew)
 where
  deplist = F.toList deps
  notifyDepChanges deps state =
    F.foldl'
      ( \(xs, state) dep ->
          let (set, newState) = notifyDepChange dep state
           in ((dep, set) : xs, newState)
      )
      ([], state)
      deps

  notifyDepChange dep oldS =
    let staleKeys = DepMap.stale dep (sifs_deps oldS)
        msg = "Change " ++ show dep ++ " invalidates " ++ show staleKeys
        maybeLog
          | HashSet.null staleKeys =
              pureDebug $
                "Nothing depends on (an old version of) " ++ show dep
          | otherwise = pureDebug msg
        (_addedKeys, newS) = invalidate staleKeys oldS
     in maybeLog (staleKeys, newS)

{- | Enqueues all given caps in the queue of stale caps an returns those that were not already
 enqueued.
-}
invalidate :: HashSet AnyCompAp -> SifState -> (HashSet AnyCompAp, SifState)
invalidate stale ges =
  let foldFun s@(newStaleCaps, curQ) x
        | x `Set.member` sifs_pendingCaps ges =
            -- don't enqueue cap that has already been dequeued for computation
            s
        | otherwise =
            case Paq.enqueue (mkPaqEntry x) curQ of
              (Paq.EnqueueAddedNewEntry, nextQ) -> (HashSet.insert x newStaleCaps, nextQ)
              (Paq.EnqueueUpdatedEntry, nextQ) -> (newStaleCaps, nextQ)
      (enqueuedCaps, newQ) = F.foldl' foldFun (HashSet.empty, sifs_stale ges) stale
   in (enqueuedCaps, ges{sifs_stale = newQ})
