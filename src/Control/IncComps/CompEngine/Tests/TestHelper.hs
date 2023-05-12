{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Control.IncComps.CompEngine.Tests.TestHelper (
  TestDep,
  TestComp,
  TestCompDef,
  runCompEngineTest,
  runCompEngineTest',
  initCompEngineTest,
  get,
  put,
  unsafeCompIO,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.CompDef
import Control.IncComps.CompEngine.CompFlowRegistry
import Control.IncComps.CompEngine.CompSink
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.CompEngine.Core
import Control.IncComps.CompEngine.Run
import Control.IncComps.CompEngine.Types
import Control.IncComps.FlowImpls.HashMapFlow
import Control.IncComps.FlowImpls.IOSink
import Control.IncComps.Utils.Fail
import Control.IncComps.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Data.Time.Clock
import Data.Typeable

type TestComp = Comp
type TestCompDef = CompDef

type TestDep = AnyCompSrcDep

hashMapSrcId :: TypedCompSrcId HashMapFlow
hashMapSrcId = typedCompSrcId (Proxy @HashMapFlow) "ident"

hashMapSinkId :: TypedCompSinkId HashMapFlow
hashMapSinkId = typedCompSinkId (Proxy @HashMapFlow) "ident"

get :: Key -> CompM (Maybe Val)
get k = compSrcReq hashMapSrcId (HashMapLookupReq k)

put :: Key -> Val -> CompM ()
put k v = compSinkReq hashMapSinkId (HashMapStoreReq k v)

runCompEngineTest
  :: (Typeable r, Show r)
  => CompDefM (Comp () r)
  -> (HashMapFlow -> ShouldStartNextRun a IO)
  -> a
  -> (HashMapFlow -> IO ())
  -> IO ()
runCompEngineTest compDefs shouldStart istate doTest =
  do
    (_hmdif, startTest) <- initCompEngineTest compDefs
    startTest CompRunUnlimitedIterations shouldStart istate doTest

initCompEngineTest
  :: (Show r, Typeable r)
  => CompDefM (Comp () r)
  -> IO
      ( HashMapFlow
      , CompRunIterationLimit
        -> (HashMapFlow -> ShouldStartNextRun a IO)
        -> a
        -> (HashMapFlow -> IO ())
        -> IO ()
      )
initCompEngineTest compDefs =
  do
    hmFlow <- initHashMapFlow "ident"
    reg <- newCompFlowRegistry
    registerCompSrc reg hmFlow
    registerCompSink reg hmFlow
    registerCompSink reg ioSink
    (_compMap, mainComp) <- failInM $ runCompDefM compDefs
    (stateIf, closeSif) <- initStateIf True
    let caps = [wrapCompAp (mkCompAp mainComp ())]
        startTest u v w x =
          runCompEngineTest' hmFlow stateIf reg caps u v w x
            `finally` closeSif
    return (hmFlow, startTest)

runCompEngineTest'
  :: HashMapFlow
  -> CompEngineStateIf IO -- FIXME: remove, there is only one state impl
  -> CompFlowRegistry
  -> [AnyCompAp]
  -> CompRunIterationLimit
  -> (HashMapFlow -> ShouldStartNextRun a IO)
  -> a
  -> (HashMapFlow -> IO ())
  -> IO ()
runCompEngineTest' hmFlow stateIf reg comps maxIterations shouldStartNextRun istate doTest =
  do
    runCompEngine compEngineIfs comps runCompEngineIf istate
    doTest hmFlow
 where
  runCompEngineIf =
    RunCompEngineIf
      { rcif_shouldStartWithRun = shouldStartNextRun hmFlow
      , rcif_emptyChangesMode = DontBlock
      , rcif_getTime = getCurrentTime
      , rcif_maxLoopRunTime = minutes 1
      , rcif_maxRunIterations = maxIterations
      , rcif_reportGarbage = garbageHandler reg
      }
  compEngineIfs =
    CompEngineIfs
      { ce_compFlowRegistry = reg
      , ce_stateIf = stateIf
      }
