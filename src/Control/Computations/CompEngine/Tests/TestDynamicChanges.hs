{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Computations.CompEngine.Tests.TestDynamicChanges (
  testGeneric,
  requestExt,
  putReq,
  RunCounters (..),
  TestReq,
  TestDep,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Fail
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import qualified Data.HashSet as HashSet
import Data.Time.Clock
import Data.Typeable

data RunCounters = RunCounters
  { rc_runs :: Int
  , rc_gets :: Int
  , rc_puts :: [String]
  }
  deriving (Show)

data TestReq a where
  TestReadReq :: TestReq String
  TestPutReq :: String -> TestReq ()

testFlowSrcId :: TypedCompSrcId TestFlow
testFlowSrcId = typedCompSrcId (Proxy @TestFlow) "TestSrc"

requestExt :: CompM String
requestExt = compSrcReq testFlowSrcId TestReadReq

putReq :: String -> CompM ()
putReq s = compSrcReq testFlowSrcId (TestPutReq s)

type TestDep = Dep String String

mkDep :: String -> TestDep
mkDep = Dep "SomeExtDep"

data TestFlow = TestFlow
  { tf_state :: TVar RunCounters
  , tf_lastChange :: TVar (Option String)
  , tf_getFun :: RunCounters -> String
  }
  deriving (Typeable)

initTestFlow :: (RunCounters -> String) -> IO TestFlow
initTestFlow getFun = do
  stateVar <- newTVarIO $ RunCounters 0 0 []
  changeVar <- newTVarIO None
  pure (TestFlow stateVar changeVar getFun)

instance CompSrc TestFlow where
  type CompSrcReq TestFlow = TestReq
  type CompSrcKey TestFlow = String
  type CompSrcVer TestFlow = String
  compSrcInstanceId _ = CompSrcInstanceId "TestSrc"
  compSrcExecute flow req =
    case req of
      TestReadReq -> do
        s' <-
          atomically $ do
            modifyTVar' (tf_state flow) (\s -> s{rc_gets = 1 + rc_gets s})
            readTVar (tf_state flow)
        logDebug ("TestDynamicChanges: read " ++ show s')
        let msg = tf_getFun flow s'
            dep = mkDep msg
        pure (HashSet.singleton dep, Ok msg)
      TestPutReq x -> do
        logDebug ("TestDynamicChanges: putting " ++ show x)
        atomically $ modifyTVar' (tf_state flow) (\s -> s{rc_puts = x : (rc_puts s)})
        pure (HashSet.empty, Ok ())
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges flow = do
    modifyTVar' (tf_state flow) (\s -> s{rc_runs = 1 + rc_runs s})
    rc <- readTVar (tf_state flow)
    oldMsg <- readTVar (tf_lastChange flow)
    let newMsg = tf_getFun flow rc
    logDebugSTM ("oldMsg=" ++ show oldMsg ++ ", newMsg=" ++ show newMsg ++ ", rc=" ++ show rc)
    changes <-
      if Some newMsg == oldMsg
        then pure HashSet.empty -- return empty set without blocking (will trigger warning)
        else do
          writeTVar (tf_lastChange flow) (Some newMsg)
          pure (HashSet.singleton (mkDep newMsg))
    logDebugSTM ("changes=" ++ show changes)
    pure changes

testGeneric
  :: (RunCounters -> String)
  -> (CompWireM (Comp () ()))
  -> Int
  -- ^ maximal number of runs
  -> IO RunCounters
testGeneric getFun getComps limit =
  do
    (_compMap, comps) <- failInM $ runCompWireM gaps
    (sif, closeSif) <- initStateIf True
    reg <- newCompFlowRegistry
    tflow <- initTestFlow getFun
    registerCompSrc reg tflow
    let ifs =
          CompEngineIfs
            { ce_compFlowRegistry = reg
            , ce_stateIf = sif
            }
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = shouldStartNextRun
            , rcif_emptyChangesMode = DontBlock
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = (seconds 10)
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = \_ -> return ()
            }
    runCompEngine ifs comps rifs ()
    closeSif
    readTVarIO (tf_state tflow)
 where
  gaps = getComps >>= \cap -> return ([wrapCompAp (mkCompAp cap ())])
  shouldStartNextRun r _ _ _ =
    return (if r < limit then startNextRun else noNextRun, ())
