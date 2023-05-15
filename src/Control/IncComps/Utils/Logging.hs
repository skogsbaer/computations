module Control.IncComps.Utils.Logging (
  LogLevel (..),
  parseLogLevel,
  allLogLevels,
  setupLogging,
  setLogLevel,
  getLogLevel,
  withLogLevel,
  logTrace,
  logDebug,
  logInfo,
  logNote,
  logWarn,
  logError,
  logNoLog,
  doLog,
  logTraceSTM,
  logDebugSTM,
  logInfoSTM,
  logNoteSTM,
  logWarnSTM,
  logErrorSTM,
  pureTrace,
  pureDebug,
  pureInfo,
  pureNote,
  pureWarn,
  pureError,
  pureNoLog,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.Utils.Ansi
import Control.IncComps.Utils.SourceLocation
import Control.IncComps.Utils.TimeUtils

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.IORef
import qualified Data.List as L
import Data.Time.Clock
import GHC.Conc (unsafeIOToSTM)
import GHC.Stack
import System.IO.Unsafe

data LogLevel
  = TRACE
  | DEBUG
  | INFO
  | NOTE
  | WARN
  | ERROR
  deriving (Eq, Ord, Show)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s =
  case L.lookup (map toLower s) logLevelTable of
    Just l -> Right l
    Nothing ->
      Left ("invalid log level, possible values: " ++ allLogLevels)

logLevelTable :: [(String, LogLevel)]
logLevelTable =
  [ ("trace", TRACE)
  , ("debug", DEBUG)
  , ("info", INFO)
  , ("note", NOTE)
  , ("warn", WARN)
  , ("error", ERROR)
  ]

allLogLevels :: String
allLogLevels = L.intercalate ", " (map fst logLevelTable)

logLevel :: IORef LogLevel
logLevel = unsafePerformIO (newIORef WARN)
{-# NOINLINE logLevel #-}

setupLogging :: LogLevel -> IO ()
setupLogging l = setLogLevel l

setLogLevel :: LogLevel -> IO ()
setLogLevel level = do
  writeIORef logLevel level

getLogLevel :: IO LogLevel
getLogLevel = readIORef logLevel

withLogLevel :: LogLevel -> IO a -> IO a
withLogLevel prio action = do
  oldPrio <- getLogLevel
  if prio == oldPrio
    then action
    else bracket (setLogLevel prio) (\_ -> setLogLevel oldPrio) (\_ -> action)

logNoLog :: MonadIO m => String -> m ()
logNoLog _msg = pure ()

color :: LogLevel -> String
color level =
  case level of
    TRACE -> darkGray
    DEBUG -> blue
    INFO -> lightBlue
    NOTE -> lightGreen
    WARN -> brown
    ERROR -> lightRed

doLog :: MonadIO m => LogLevel -> CallStack -> String -> m ()
doLog level stack msg = liftIO $ do
  enabledLevel <- readIORef logLevel
  when (level >= enabledLevel) $ do
    t <- getCurrentTime
    let ts = take 23 (formatUTCTimeHiRes' t) -- only millis
        loc = callerSrcLoc stack
        file = srcLocFile loc
        line = srcLocStartLine loc
        locStr = file ++ ":" ++ show line
        full = "[" ++ ts ++ " " ++ show level ++ " " ++ locStr ++ "] " ++ msg
    putStrLn (color level ++ full ++ reset)

logTrace :: (HasCallStack, MonadIO m) => String -> m ()
logTrace msg = liftIO $ doLog TRACE callStack msg

logDebug :: (HasCallStack, MonadIO m) => String -> m ()
logDebug msg = liftIO $ doLog DEBUG callStack msg

logInfo :: (HasCallStack, MonadIO m) => String -> m ()
logInfo msg = liftIO $ doLog INFO callStack msg

logNote :: (HasCallStack, MonadIO m) => String -> m ()
logNote msg = liftIO $ doLog NOTE callStack msg

logWarn :: (HasCallStack, MonadIO m) => String -> m ()
logWarn msg = liftIO $ doLog WARN callStack msg

logError :: (HasCallStack, MonadIO m) => String -> m ()
logError msg = liftIO $ doLog ERROR callStack msg

doLogSTM :: LogLevel -> CallStack -> String -> STM ()
doLogSTM level loc msg = unsafeIOToSTM (doLog level loc msg)

logTraceSTM :: (HasCallStack) => String -> STM ()
logTraceSTM msg = doLogSTM TRACE callStack msg

logDebugSTM :: (HasCallStack) => String -> STM ()
logDebugSTM msg = doLogSTM DEBUG callStack msg

logInfoSTM :: (HasCallStack) => String -> STM ()
logInfoSTM msg = doLogSTM INFO callStack msg

logNoteSTM :: (HasCallStack) => String -> STM ()
logNoteSTM msg = doLogSTM NOTE callStack msg

logWarnSTM :: (HasCallStack) => String -> STM ()
logWarnSTM msg = doLogSTM WARN callStack msg

logErrorSTM :: (HasCallStack) => String -> STM ()
logErrorSTM msg = doLogSTM ERROR callStack msg

doLogPure :: LogLevel -> CallStack -> String -> a -> a
doLogPure level loc msg x =
  unsafePerformIO (doLog level loc msg >> pure x)

pureTrace :: (HasCallStack) => String -> a -> a
pureTrace msg = doLogPure TRACE callStack msg

pureDebug :: (HasCallStack) => String -> a -> a
pureDebug msg = doLogPure DEBUG callStack msg

pureInfo :: (HasCallStack) => String -> a -> a
pureInfo msg = doLogPure INFO callStack msg

pureNote :: (HasCallStack) => String -> a -> a
pureNote msg = doLogPure NOTE callStack msg

pureWarn :: (HasCallStack) => String -> a -> a
pureWarn msg = doLogPure WARN callStack msg

pureError :: (HasCallStack) => String -> a -> a
pureError msg = doLogPure ERROR callStack msg

pureNoLog :: String -> a -> a
pureNoLog _ x = x
