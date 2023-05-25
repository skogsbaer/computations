{-# LANGUAGE TypeFamilies #-}

module Control.Computations.FlowImpls.CompLogging (
  doLogC,
  logTraceC,
  logDebugC,
  logInfoC,
  logNoteC,
  logWarnC,
  logErrorC,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.Types
import Control.Computations.FlowImpls.IOSink
import Control.Computations.Utils.Logging

----------------------------------------
-- EXTERNAL
----------------------------------------
import GHC.Stack

doLogC :: LogLevel -> CallStack -> String -> CompM ()
doLogC level stack msg = unsafeCompIO (doLog level stack msg)

logTraceC :: (HasCallStack) => String -> CompM ()
logTraceC msg = doLogC TRACE callStack msg

logDebugC :: (HasCallStack) => String -> CompM ()
logDebugC msg = doLogC DEBUG callStack msg

logInfoC :: (HasCallStack) => String -> CompM ()
logInfoC msg = doLogC INFO callStack msg

logNoteC :: (HasCallStack) => String -> CompM ()
logNoteC msg = doLogC NOTE callStack msg

logWarnC :: (HasCallStack) => String -> CompM ()
logWarnC msg = doLogC WARN callStack msg

logErrorC :: (HasCallStack) => String -> CompM ()
logErrorC msg = doLogC ERROR callStack msg
