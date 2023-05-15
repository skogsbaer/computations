module Control.IncComps.Demos.DirSync.Main (
  syncDirs,
  syncDirs',
  RunStats (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine
import Control.IncComps.FlowImpls.CompLogging
import Control.IncComps.FlowImpls.FileSink
import Control.IncComps.FlowImpls.FileSrc
import Control.IncComps.FlowImpls.IOSink
import Control.IncComps.Utils.Fail
import Control.IncComps.Utils.IOUtils
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashSet (HashSet)
import Data.Proxy
import Data.Time.Clock
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

fileSrcId :: TypedCompSrcId FileSrc
fileSrcId = typedCompSrcId (Proxy @FileSrc) "fileSrc"

fileSinkId :: TypedCompSinkId FileSink
fileSinkId = typedCompSinkId (Proxy @FileSink) "fileSink"

readFile :: FilePath -> CompM BS.ByteString
readFile p = compSrcReq fileSrcId (ReadFile p)

writeFile :: FilePath -> BS.ByteString -> CompM ()
writeFile p bs = compSinkReq fileSinkId (WriteFile p bs)

listDir :: FilePath -> CompM (HashSet DirEntry)
listDir p = compSrcReq fileSrcId (ListDir p)

mkdir :: FilePath -> CompM ()
mkdir p = compSinkReq fileSinkId (MakeDirs p)

fileSyncCompDef :: FilePath -> CompDef FilePath ()
fileSyncCompDef src = mkComp "fileSyncComp" inMemoryLHCaching $ \path ->
  do
    bs <- readFile (src </> path)
    writeFile path bs

dirSyncCompDef :: FilePath -> Comp FilePath () -> Comp FilePath () -> CompDef FilePath ()
dirSyncCompDef src fileComp dirComp = mkComp "dirSyncComp" inMemoryLHCaching $ \path ->
  do
    mkdir path
    contents <- listDir (src </> path)
    forM_ contents $ \entry ->
      case de_type entry of
        RegularFileType -> void $ evalComp fileComp (path </> de_name entry)
        DirectoryFileType ->
          do
            let subdir = path </> de_name entry
            void $ evalComp dirComp subdir
        t ->
          logDebugC
            ( "Ignoring directory entry "
                ++ de_name entry
                ++ " of "
                ++ path
                ++ " with type "
                ++ show t
            )

syncDirs :: FilePath -> FilePath -> IO ()
syncDirs src tgt =
  do
    runVar <- newTVarIO None
    syncDirs' runVar src tgt

data RunStats = RunStats
  { rs_run :: Int
  , rs_hadChanges :: Bool
  , rs_staleCaps :: Int
  }

syncDirs' :: TVar (Option RunStats) -> FilePath -> FilePath -> IO ()
syncDirs' runVar src' tgt' =
  do
    src <- canonicalizePath src'
    tgt <- canonicalizePath tgt'
    reg <- newCompFlowRegistry
    withStateIf $ \stateIf ->
      withFileSrc (defaultFileSrcConfig "fileSrc") $ \fileSrc ->
        do
          fileSink <- makeFileSink "fileSink" tgt
          registerCompSrc reg fileSrc
          registerCompSink reg fileSink
          registerCompSink reg ioSink
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
          comps <- rootComps src
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
  rootComps src = (failInM . fmap snd) $
    runCompDefM $
      do
        c <- defineComps src
        pure ([wrapCompAp (mkCompAp c ".")])
  defineComps :: FilePath -> CompDefM (Comp FilePath ())
  defineComps src =
    do
      fileComp <- defineComp (fileSyncCompDef src)
      defineRecursiveComp (dirSyncCompDef src fileComp)
