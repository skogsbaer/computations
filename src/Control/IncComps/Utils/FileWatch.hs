{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.IncComps.Utils.FileWatch (
  FileWatch,
  initFileWatch,
  closeFileWatch,
  withFileWatch,
  watchFile,
  unwatchFile,
  FileChange (..),
  waitForFileChanges,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.Utils.Clock
import Control.IncComps.Utils.ConcUtils
import Control.IncComps.Utils.IOUtils
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import Test.Framework

data FileWatch = FileWatch
  { fw_thread :: Async ()
  , fw_watchedFiles :: TVar (Map CanonPath (Option FileStatus))
  -- ^ All files being watched
  , fw_notifications :: TVar (Map CanonPath FileChange)
  -- ^ Set of pending notifications
  }

initFileWatch :: Clock -> TimeSpan -> IO FileWatch
initFileWatch clock waitInterval = do
  filesVar <- newTVarIO Map.empty
  notifyVar <- newTVarIO Map.empty
  let loop = do
        c_sleep clock waitInterval
        fileWatchCheck filesVar notifyVar
        loop
  thread <- async loop
  pure (FileWatch thread filesVar notifyVar)

closeFileWatch :: FileWatch -> IO ()
closeFileWatch fw = cancel (fw_thread fw)

withFileWatch :: Clock -> TimeSpan -> (FileWatch -> IO a) -> IO a
withFileWatch clock waitInterval action =
  bracket (initFileWatch clock waitInterval) closeFileWatch action

fileWatchCheck :: TVar (Map CanonPath (Option FileStatus)) -> TVar (Map CanonPath FileChange) -> IO ()
fileWatchCheck filesVar notifyVar = do
  logTrace "Doing file watch check ..."
  old <- readTVarIO filesVar
  newChanges <-
    fmap (Map.fromList . catMaybes) $ forM (Map.toList old) $ \(path, oldStatus) -> do
      newStatus <- safeGetFileStatus (unCanonPath path)
      pure $
        if oldStatus == newStatus
          then Nothing
          else Just (path, FileChange path oldStatus newStatus)
  if null newChanges
    then logTrace "No new changes"
    else logDebug ("New changes: " ++ show newChanges)
  let newFiles = Map.map fc_new newChanges
  -- update list of watched vars. Do not use old to avoid race conditions. Prefer
  -- new when building the union.
  atomically $ modifyTVar' filesVar $ \oldFiles -> Map.union newFiles oldFiles
  -- Update the set of notifications. We need to keep and update existing notifications
  -- because these notifications have not been collect yet.
  atomically $ modifyTVar' notifyVar $ \oldChanges ->
    Map.unionWith combineChange newChanges oldChanges
 where
  combineChange newChange oldChange =
    FileChange
      { fc_path = fc_path newChange
      , fc_old = fc_old oldChange
      , fc_new = fc_new newChange
      }

watchFile :: FileWatch -> FilePath -> IO CanonPath
watchFile fw path = do
  normPath <- canonPath path
  logDebug ("Watching file " ++ path)
  atomically $ modifyTVar' (fw_watchedFiles fw) $ Map.insertWith (\old _new -> old) normPath None
  pure normPath

unwatchFile :: FileWatch -> CanonPath -> IO ()
unwatchFile fw path = do
  logDebug ("No longer watching file " ++ unCanonPath path)
  atomically $ modifyTVar' (fw_watchedFiles fw) $ Map.delete path

data FileChange = FileChange
  { fc_path :: CanonPath
  , fc_old :: Option FileStatus
  , fc_new :: Option FileStatus
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

waitForFileChanges :: FileWatch -> STM (HashSet FileChange)
waitForFileChanges fw = do
  changes <- readTVar (fw_notifications fw)
  if Map.null changes
    then retry
    else do
      writeTVar (fw_notifications fw) Map.empty
      logDebugSTM ("Got file changes: " ++ show changes)
      pure (HashSet.fromList (Map.elems changes))

--
-- Tests
--
test_basics :: IO ()
test_basics = loop 1 -- run basicTest several times because it's not deterministic
 where
  numRuns = 10
  loop :: Int -> IO ()
  loop n = do
    logNote ("Test run " ++ show n)
    basicTest
    when (n <= numRuns) $ loop (n + 1)

basicTest :: IO ()
basicTest =
  withFileWatch realClock pollInterval $ \fw -> withSysTempDir $ \root' -> do
    root <- canonicalizePath root'
    p1 <- canonicalizePath (root </> "file1")
    p2 <- canonicalizePath (root </> "file2")
    t1 <- c_currentTime realClock
    void $ watchFile fw p1
    void $ watchFile fw p2

    logInfo ("First change to p1")
    writeFile p1 "abc"
    [c1] <- getChanges fw
    t2 <- subAssert $ assertFileChange c1 p1 None (Some t1)

    logInfo ("First change to p2")
    writeFile p2 "xyz"
    [c2] <- getChanges fw
    t3 <- subAssert $ assertFileChange c2 p2 None t2

    logInfo ("Second change to p2")
    writeFile p2 "123"
    [c3] <- getChanges fw
    t4 <- subAssert $ assertFileChange c3 p2 t3 t3

    logInfo ("Also watch directory, change both files")
    void $ watchFile fw root
    writeFile p1 "yuck"
    writeFile p2 "42"
    [cRoot, cP1, cP2] <- getChanges fw
    t5 <- subAssert $ assertFileChange cP1 p1 t2 t2
    t6 <- subAssert $ assertFileChange cRoot root None t2
    _ <- subAssert $ assertFileChange cP2 p2 t4 t4

    logInfo ("Change p1 again") -- should not trigger change for directory
    writeFile p1 "x"
    [c4] <- getChanges fw
    _ <- subAssert $ assertFileChange c4 p1 t5 t5

    logInfo ("Create new file")
    writeFile (root </> "file3") "x"
    [c5] <- getChanges fw
    _ <- subAssert $ assertFileChange c5 root t6 t6
    pure ()
 where
  assertFileChange
    :: FileChange -> FilePath -> Option UTCTime -> Option UTCTime -> IO (Option UTCTime)
  assertFileChange fc path old new = do
    assertEqual path (unCanonPath (fc_path fc))
    subAssert $ assertStatus path old (fc_old fc) (==) "equal to"
    subAssert $ assertStatus path new (fc_new fc) (<) "smaller than"
    pure (fmap (posixSecondsToUTCTime . fs_mtime) (fc_new fc))
  assertStatus
    :: FilePath
    -> Option UTCTime
    -> Option FileStatus
    -> (UTCTime -> UTCTime -> Bool)
    -> String
    -> IO ()
  assertStatus path old new cmp cpmDescr =
    case (old, new) of
      (None, None) -> pure ()
      (Some t, Some status) -> do
        let statusMtime = posixSecondsToUTCTime (fs_mtime status)
            msg =
              "Expected timestamp "
                ++ formatUTCTimeHiRes t
                ++ " is not "
                ++ cpmDescr
                ++ " mtime "
                ++ formatUTCTimeHiRes statusMtime
                ++ " of "
                ++ path
        assertBoolVerbose msg (cmp t statusMtime)
      (a, b) ->
        assertFailure
          ( "File status "
              ++ show b
              ++ " does not match expected mtime "
              ++ show a
          )
  getChanges fw = do
    c_sleep realClock (2 * pollInterval)
    timeoutFail "waitForFileChanges should be prompt" (seconds 1) $ atomically $ do
      changeSet <- waitForFileChanges fw
      pure (sort (HashSet.toList changeSet))
  pollInterval = milliseconds 10
