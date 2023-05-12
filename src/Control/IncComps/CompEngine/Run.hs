{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.IncComps.CompEngine.Run (
  Blocking (..),
  RunCompEngineIf (..),
  RunSettings (..),
  NextRun (..),
  noNextRun,
  startNextRun,
  ShouldStartNextRun,
  CompRunIterationLimit (..),
  initStateIf,
  withStateIf,
  runCompEngine,
  garbageHandler,
  deleteDeadOutputs,
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine.CompFlow
import Control.IncComps.CompEngine.CompFlowRegistry
import Control.IncComps.CompEngine.CompSink
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.CompEngine.Core
import qualified Control.IncComps.CompEngine.Impl as Impl
import Control.IncComps.CompEngine.SimpleStateIf
import Control.IncComps.CompEngine.Types
import Control.IncComps.Utils.Logging
import qualified Control.IncComps.Utils.MultiMap as MM
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Data.Word
import GHC.Stack

newtype RunSettings = RunSettings
  { rs_maxRunIterations :: Option CompRunIterationLimit
  -- ^ overwrites 'rcif_maxRunIterations' (from RunCompEngineIf) for a single run if set.
  -- Useful for tests.
  }
  deriving (Show, Eq)

defaultRunSettings :: RunSettings
defaultRunSettings =
  RunSettings
    { rs_maxRunIterations = None
    }

data NextRun
  = NoNextRun
  | StartNextRun RunSettings
  deriving (Show, Eq)

noNextRun :: NextRun
noNextRun = NoNextRun

startNextRun :: NextRun
startNextRun = StartNextRun defaultRunSettings

type ShouldStartNextRun a m =
  Int
  -- ^ number of the next run
  -> Bool
  -- ^ true if changes were found in the last run
  -> Int
  -- ^ stale caps from last run
  -> a
  -- ^ state returned from the last check
  -> m (NextRun, a)
  -- ^ return 'StartNextRun' and a new state if the run loop should continue

withStateIf
  :: (CompEngineStateIf IO -> IO a)
  -> IO a
withStateIf action =
  bracket
    (initStateIf False)
    (\(_, stop) -> liftIO stop)
    (action . fst)

initStateIf
  :: Bool
  -> IO (CompEngineStateIf IO, IO ())
initStateIf shouldValidate =
  do
    stateIf <- setupSimpleStateIf shouldValidate
    exitMVar <- liftIO newEmptyMVar
    let close = liftIO (putMVar exitMVar ())
    return (stateIf, close)

setupSimpleStateIf
  :: Bool
  -> IO (CompEngineStateIf IO)
setupSimpleStateIf shouldValidate =
  do
    var <- newTVarIO initialSifState
    let stateIf =
          SimpleStateIf
            { ssif_withState =
                \f ->
                  do
                    (x, !newState) <-
                      atomically $
                        do
                          oldState <- readTVar var
                          let res = f oldState
                          writeTVar var (snd res) -- lazy, forced later
                          return res
                    when shouldValidate $ validateSifState newState
                    return x
            }
    return (mkSimpleCompEngineStateIf stateIf)

data RunCompEngineIf a = RunCompEngineIf
  { rcif_shouldStartWithRun :: ShouldStartNextRun a IO
  -- ^ blocks or returns if run loop should continue
  , rcif_emptyChangesMode :: Blocking
  -- ^ whether `allCompSrcChanges` should block if there are no changes
  , rcif_getTime :: IO UTCTime
  , rcif_maxLoopRunTime :: TimeSpan
  -- ^ maximum time the processing loop runs without checking for new changes
  , rcif_maxRunIterations :: CompRunIterationLimit
  , --  ^ maximum number of iterations the processing loop runs without checking for new changes
    rcif_reportGarbage :: Garbage -> IO ()
  }

data CompRunIterationLimit
  = CompRunLimitIterationsTo Word64
  | CompRunUnlimitedIterations
  deriving (Eq, Show)

runCompEngine
  :: forall a
   . CompEngineIfs
  -> [AnyCompAp]
  -> RunCompEngineIf a
  -> a
  -> IO ()
runCompEngine ceIfs comps rcif startState =
  main ceIfs
 where
  main :: CompEngineIfs -> IO ()
  main ifs =
    do
      ce <- Impl.startCompEngine ifs comps
      loop ce 1 startState True 0 mempty
      Impl.stopCompEngine ce
      return ()
  loop
    :: Impl.CompEngine
    -> Int
    -> a
    -> Bool
    -> Int
    -> Impl.GenDel
    -> IO ()
  loop ce !run !lastState !hadChanges !staleCapsFromLastRun !lastG =
    do
      (continue, newState) <-
        rcif_shouldStartWithRun rcif run hadChanges staleCapsFromLastRun lastState
      case continue of
        NoNextRun -> logNote "Stopping CompEngine loop due to user request."
        StartNextRun runSettings -> do
          logDebug
            ( "Preparing run "
                ++ show run
                ++ " of CompEngine loop, hadChanges="
                ++ show hadChanges
                ++ ", staleCapsFromLastRun="
                ++ show staleCapsFromLastRun
                ++ ", emptyChangesMode="
                ++ show (rcif_emptyChangesMode rcif)
            )
          loop' ce run newState staleCapsFromLastRun lastG runSettings
  loop' ce run newState staleCapsFromLastRun lastG runSettings =
    do
      changes <- do
        let b =
              if staleCapsFromLastRun > 0
                then DontBlock
                else rcif_emptyChangesMode rcif
        logDebug ("Waiting for changes, blocking=" ++ show b)
        atomically $ allCompSrcChanges (ce_compFlowRegistry ceIfs) b
      let lenChanges = HashSet.size changes
          changesize = show lenChanges ++ " changes"
          changesList = HashSet.toList changes
          changesByType = MM.fromList [(identifyType x, x) | x <- changesList]
          maxRunIterations =
            fromOption (rcif_maxRunIterations rcif) (rs_maxRunIterations runSettings)
          changerepr =
            case changesList of
              [] -> "no change"
              [x] -> "change " ++ show x
              _ -> changesize
          logChange :: forall a. (Show a, HasCallStack) => LogLevel -> String -> a -> IO ()
          logChange level prefix =
            doLog level callStack . ((prefix ++ ": ") ++) . show
      if null changes then logDebug "Found no changes" else logInfo ("Found " ++ changerepr)
      forM_ (MM.toSetList changesByType) $ \(ty, changesOfTy@(HashSet.size -> changesSize)) ->
        if changesSize < 10
          then mapM_ (logChange INFO "  - ") changesOfTy
          else do
            let changesList = (HashSet.toList changesOfTy)
            logInfo
              ( " "
                  ++ T.unpack (unTypeId ty)
                  ++ " has "
                  ++ show changesSize
                  ++ " changes. "
                  ++ "Here are 5 of them:"
              )
            mapM_ (logChange INFO "  - ") (take 5 changesList)
            logDebug "Here are the remaining: "
            mapM_ (logChange DEBUG "  - ") (drop 5 changesList)
      (hasStaleCaps, nextG) <-
        if not (null changes) || staleCapsFromLastRun > 0
          then do
            enqInfo <- Impl.notifyCompEngine ce changesList
            logInfo
              ( "Notified CompEngine about changes. "
                  ++ show (Map.size (ei_affectedCaps enqInfo))
                  ++ " new stale caps."
              )
            logStale changerepr (Map.keys (ei_affectedCaps enqInfo))
            let remBefore = ei_currentQueueSize enqInfo
            logNote
              ( "Starting run "
                  ++ show run
                  ++ " of CompEngine with "
                  ++ show remBefore
                  ++ " stale caps."
              )
            t0 <- rcif_getTime rcif
            let innerLoop state@(fromIntegral @Int -> i, _) g =
                  rcif_getTime rcif >>= \t ->
                    if ( t
                          `diffTime` t0
                          > rcif_maxLoopRunTime rcif
                          || CompRunLimitIterationsTo i
                          == maxRunIterations
                       )
                      then return (state, g)
                      else continueLoop state g
                continueLoop (i, _) g =
                  do
                    (r, newG) <- Impl.stepCompEngine ce g
                    if r < 0
                      then return ((i, 0), newG)
                      else innerLoop (i + 1, r) newG
                s0 = (0, remBefore)
            ((iterations, remAfter), g) <- innerLoop s0 lastG
            nextG <-
              if (remAfter <= 0)
                then do
                  rcif_reportGarbage rcif (Impl.garbage g)
                  return mempty
                else return g
            logInfo
              ( "Finished run "
                  ++ show run
                  ++ " with "
                  ++ show iterations
                  ++ " iterations and "
                  ++ show remAfter
                  ++ " stale caps remaining."
              )
            return (remAfter, nextG)
          else return (0, mempty)
      let haveChanges = not (null changes)
      loop ce (run + 1) newState haveChanges hasStaleCaps nextG

garbageHandler :: CompFlowRegistry -> Garbage -> IO ()
garbageHandler reg g =
  do
    let garbageMap = unionsAnyCompSinkOutsMap $ HashMap.elems $ garbage_outputs g
    forM_ (anyOutsMapToList garbageMap) $ \(_, anyOutputs) ->
      withCompSinkForOuts reg anyOutputs $ \sink xs ->
        do
          logInfo $
            "Deleting "
              ++ show (HashSet.size xs)
              ++ " outputs of "
              ++ show (compSinkId sink)
          compSinkDeleteOutputs sink xs
    forAllSrcs_ reg srcFun
 where
  srcFun :: forall s. CompSrc s => s -> IO ()
  srcFun src = do
    let key = compSrcId src
        deps =
          -- list monad
          do
            (ForAnyCompFlow ident _ depKey) <- F.toList (garbage_deps g)
            guard (ident == key)
            case cast depKey of
              Nothing -> []
              Just (SomeCompSrcKey d :: SomeCompSrcKey s) -> [d]
    case deps of
      [] -> logDebug $ "No deps to delete for " <> show key
      _ ->
        do
          let depsSet = HashSet.fromList deps
          logInfo $
            "Deleting " ++ show (HashSet.size depsSet) ++ " deps of " ++ show key
          compSrcUnregister src depsSet

withCompSinkForOuts
  :: forall a
   . CompFlowRegistry
  -> AnyCompSinkOuts
  -> (forall s. CompSink s => s -> CompSinkOuts s -> IO a)
  -> IO a
withCompSinkForOuts reg (ForAnyCompFlow id _ someOuts) fun = go someOuts
 where
  go :: forall s. CompSink s => SomeCompSinkOuts s -> IO a
  go (SomeCompSinkOuts outs) = withCompSinkId reg id more >>= failInM
   where
    more :: s -> IO a
    more sink = fun sink outs

{- | Asks the StateIf which outputs currently live and asks the DataIf
 which outputs exists and deletes all those that exist but aren't alive.
-}
deleteDeadOutputs :: CompSink s => CompEngineStateIf IO -> s -> IO ()
deleteDeadOutputs stateIf sink =
  case compSinkListExistingOutputs sink of
    None ->
      logNote $
        "CompSink "
          ++ show (compSinkId sink)
          ++ " doesn't support deleting dead outputs. Not deleting anything."
    Some listAction ->
      do
        aliveOutputs <- getCompSinkOuts stateIf sink
        diskOutputs <- listAction
        let deadOutputs = diskOutputs `HashSet.difference` aliveOutputs
            diskSize = HashSet.size diskOutputs
            deadSize = HashSet.size deadOutputs
            aliveSize = HashSet.size aliveOutputs
            sinkName = show (compSinkId sink)
        logInfo
          ( sinkName
              ++ " has "
              ++ show diskSize
              ++ " outputs on disk, "
              ++ show aliveSize
              ++ " alive"
          )
        when (diskSize > 0) $ logDebug ("On disk (first 10): " ++ show (take 10 (HashSet.toList diskOutputs)))
        when (aliveSize > 0) $ logDebug ("Alive (first 10): " ++ show (take 10 (HashSet.toList aliveOutputs)))
        unless (HashSet.null deadOutputs) $
          do
            logNote
              ( sinkName
                  ++ ": Deleting "
                  ++ show deadSize
                  ++ " outputs that exist but are no longer produced."
              )
            logDebug ("Dead (first 10): " ++ show (take 10 (HashSet.toList deadOutputs)))
            compSinkDeleteOutputs sink deadOutputs
