module Control.IncComps.CompEngine.Driver (
  RunStats (..),
  compDriver,
  regSrc,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.CompDef
import Control.IncComps.CompEngine.CompFlowRegistry
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.CompEngine.Core
import Control.IncComps.CompEngine.Run
import Control.IncComps.CompEngine.Types
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.Types

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
  => TVar (Option RunStats)
  -> (CompFlowRegistry -> IO () -> IO a)
  -> CompDefM (Comp p r)
  -> p
  -> IO a
compDriver runVar withRegisteredFlows defineComps startVal = do
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
    runCompDefM $
      do
        c <- defineComps
        pure ([wrapCompAp (mkCompAp c startVal)])

regSrc :: CompSrc s => CompFlowRegistry -> IO a -> s -> IO a
regSrc reg action src = do
  registerCompSrc reg src
  action
