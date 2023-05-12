{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.IncComps.CompEngine.CompFlowRegistry (
  CompFlowRegistry,
  newCompFlowRegistry,
  withCompSinkId,
  withTypedCompSinkId,
  withTypedCompSrcId,
  forAllSrcs_,
  forAllSinks_,
  registerCompSrc,
  unregisterCompSrc,
  registerCompSink,
  unregisterCompSink,
  allCompSrcChanges,
  Blocking (..),
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine.CompSink
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.Utils.Fail
import Control.IncComps.Utils.Logging

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Proxy
import Data.Typeable
import Test.Framework

data RegState = RegState
  { rs_srcs :: HashMap CompSrcId AnyCompSrc
  , rs_sinks :: HashMap CompSinkId AnyCompSink
  }

newtype CompFlowRegistry = CompFlowRegistry (TVar RegState)

newCompFlowRegistry :: IO CompFlowRegistry
newCompFlowRegistry = do
  let state = RegState HashMap.empty HashMap.empty
  v <- newTVarIO state
  pure (CompFlowRegistry v)

withTypedCompSrcId
  :: forall m a s
   . (MonadIO m, CompSrc s)
  => CompFlowRegistry
  -> TypedCompSrcId s
  -> (s -> m a)
  -> m (Fail a)
withTypedCompSrcId reg (TypedCompSrcId key) fun =
  withCompSrcId reg key fun

withCompSrcId
  :: forall m a s
   . (MonadIO m, CompSrc s)
  => CompFlowRegistry
  -> CompSrcId
  -> (s -> m a)
  -> m (Fail a)
withCompSrcId (CompFlowRegistry var) key fun = do
  reg <- liftIO $ readTVarIO var
  case Map.lookup key (rs_srcs reg) of
    Just (AnyCompSrc src) ->
      case cast src of
        Just src' -> Ok <$> fun src'
        Nothing ->
          pure $
            Fail
              ( "Expected CompSrc of type "
                  ++ show (typeRep (Proxy :: Proxy s))
                  ++ " but got: "
                  ++ show (compSrcId src)
              )
    Nothing ->
      do
        let msg = "No CompSrc registered for key " ++ show key
        logWarn msg
        pure $ Fail msg

withTypedCompSinkId
  :: forall m a s
   . (MonadIO m, CompSink s)
  => CompFlowRegistry
  -> TypedCompSinkId s
  -> (s -> m a)
  -> m (Fail a)
withTypedCompSinkId reg (TypedCompSinkId key) fun =
  withCompSinkId reg key fun

withCompSinkId
  :: forall m a s
   . (MonadIO m, CompSink s)
  => CompFlowRegistry
  -> CompSinkId
  -> (s -> m a)
  -> m (Fail a)
withCompSinkId (CompFlowRegistry var) key fun = do
  reg <- liftIO $ readTVarIO var
  case Map.lookup key (rs_sinks reg) of
    Just (AnyCompSink sink) ->
      case cast sink of
        Just sink' -> Ok <$> fun sink'
        Nothing ->
          pure $
            Fail
              ( "Expected CompSink of type "
                  ++ show (typeRep (Proxy :: Proxy s))
                  ++ " but got: "
                  ++ show (compSinkId sink)
              )
    Nothing ->
      do
        let msg = "No CompSink registered for key " ++ show key
        logWarn msg
        pure $ Fail msg

forAllSrcs_
  :: MonadIO m
  => CompFlowRegistry
  -> (forall s. CompSrc s => s -> m ())
  -> m ()
forAllSrcs_ (CompFlowRegistry var) srcFun =
  do
    reg <- liftIO $ readTVarIO var
    F.forM_ (rs_srcs reg) $ \(AnyCompSrc src) -> srcFun src

forAllSinks_
  :: MonadIO m
  => CompFlowRegistry
  -> (forall s. CompSink s => s -> m ())
  -> m ()
forAllSinks_ (CompFlowRegistry var) sinkFun =
  do
    reg <- liftIO $ readTVarIO var
    F.forM_ (rs_sinks reg) $ \(AnyCompSink sink) -> sinkFun sink

registerCompSrc :: CompSrc s => CompFlowRegistry -> s -> IO ()
registerCompSrc (CompFlowRegistry var) src =
  do
    logInfo ("Registering " ++ show (compSrcId src))
    atomically $ modifyTVar' var $ \state ->
      state{rs_srcs = HashMap.insert (compSrcId src) (AnyCompSrc src) (rs_srcs state)}

unregisterCompSrc :: CompSrc s => CompFlowRegistry -> s -> IO ()
unregisterCompSrc (CompFlowRegistry var) src =
  do
    logInfo ("Unregistering " ++ show (compSrcId src))
    atomically $ modifyTVar' var $ \state ->
      state{rs_srcs = HashMap.delete (compSrcId src) (rs_srcs state)}

registerCompSink :: CompSink s => CompFlowRegistry -> s -> IO ()
registerCompSink (CompFlowRegistry var) sink =
  do
    logInfo ("Registering " ++ show (compSinkId sink))
    atomically $ modifyTVar' var $ \state ->
      state{rs_sinks = HashMap.insert (compSinkId sink) (AnyCompSink sink) (rs_sinks state)}

unregisterCompSink :: CompSink s => CompFlowRegistry -> s -> IO ()
unregisterCompSink (CompFlowRegistry var) sink =
  do
    logInfo ("Unregistering " ++ show (compSinkId sink))
    atomically $ modifyTVar' var $ \state ->
      state{rs_sinks = HashMap.delete (compSinkId sink) (rs_sinks state)}

data Blocking = Block | DontBlock
  deriving (Eq, Show)

allCompSrcChanges :: CompFlowRegistry -> Blocking -> STM (HashSet AnyCompSrcDep)
allCompSrcChanges (CompFlowRegistry var) b =
  do
    reg <- readTVar var
    let srcs = rs_srcs reg
    logDebugSTM ("Waiting for changes on " ++ show (Map.size srcs) ++ " sources")
    let changesActions :: [STM (HashSet AnyCompSrcDep)]
        changesActions = map (\(AnyCompSrc src) -> getChanges src) (HashMap.elems srcs)
    -- wait for at least one change
    set1 <-
      case b of
        Block -> F.foldl' orElse retry changesActions
        DontBlock -> pure HashSet.empty
    -- collect all changes
    sets <- mapM (`orElse` pure HashSet.empty) changesActions
    pure (HashSet.unions (set1 : sets))
 where
  getChanges :: CompSrc s => s -> STM (HashSet AnyCompSrcDep)
  getChanges s =
    do
      deps <- compSrcWaitChanges s
      when (HashSet.null deps) $
        logDebugSTM (show (compSrcId s) ++ " returned no results, it should have blocked in STM.")
      pure (HashSet.map (wrapCompSrcDep s) deps)
