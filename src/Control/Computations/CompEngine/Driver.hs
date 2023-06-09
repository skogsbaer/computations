module Control.Computations.CompEngine.Driver (
  RunStats (..),
  compDriver,
  compDriver',
  regSrc,
  regSink,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Data.Time.Clock

data RunStats = RunStats
  { rs_run :: Int
  , rs_hadChanges :: Bool
  , rs_staleCaps :: Int
  }

compDriver
  :: (IsCompParam p, IsCompResult r)
  => (CompFlowRegistry -> IO () -> IO ())
  -> CompWireM (Comp p r)
  -> p
  -> IO ()
compDriver withRegisteredFlows wireComps startVal = do
  runVar <- newTVarIO None
  compDriver' runVar withRegisteredFlows wireComps startVal

compDriver'
  :: (IsCompParam p, IsCompResult r)
  => TVar (Option RunStats)
  -> (CompFlowRegistry -> IO () -> IO ())
  -> CompWireM (Comp p r)
  -> p
  -> IO ()
compDriver' runVar withRegisteredFlows wireComps startVal = do
  reg <- newCompFlowRegistry
  withStateIf $ \stateIf -> withRegisteredFlows reg $ do
    let ifs =
          CompEngineIfs
            { ce_compFlowRegistry = reg
            , ce_stateIf = stateIf
            }
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = shouldStartNextRun stateIf reg runVar
            , rcif_emptyChangesMode = Block
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = (seconds 10)
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = garbageHandler reg
            }
    comps <- rootComps
    runCompEngine ifs comps rifs ()
 where
  runDeletes stateIf reg =
    do
      logNote ("Deleting leftovers from previous program run")
      forAllSinks_ reg (deleteDeadOutputs stateIf)
  shouldStartNextRun stateIf reg runVar nRun hadChanges nStaleCaps state =
    do
      when (nRun == 1) (runDeletes stateIf reg)
      let !stats = RunStats{rs_run = nRun, rs_hadChanges = hadChanges, rs_staleCaps = nStaleCaps}
      atomically $ writeTVar runVar (Some stats)
      pure (startNextRun, state)
  rootComps = (failInM . fmap snd) $
    runCompWireM $
      do
        c <- wireComps
        pure ([wrapCompAp (mkCompAp c startVal)])

regSrc :: CompSrc s => CompFlowRegistry -> IO a -> s -> IO a
regSrc reg action src = do
  registerCompSrc reg src
  action

regSink :: CompSink s => CompFlowRegistry -> IO a -> s -> IO a
regSink reg action sink = do
  registerCompSink reg sink
  action
