{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.FileStore.Tests (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.FileStore.Watcher
import Control.Computations.Utils.FileStore.Writer
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Test.Framework

test_basics :: IO ()
test_basics = withLogLevel DEBUG $
  withSysTempDir $ \rootDir -> withFsAndWatcher rootDir pollInterval $ \(fs, fsw) -> do
    logNote "Starting test ..."
    v1 <- storeDoc fs did1 bs1
    assertEqual (mkVersion 1) v1
    v1' <- storeDoc fs did1 bs1 -- store with same content
    assertEqual v1 v1'
    v2 <- storeDoc fs did1 bs2 -- store with different content
    assertEqual (mkVersion 2) v2
    v3 <- storeDoc fs did2 bs3 -- store other doc
    assertEqual (mkVersion 1) v3
    Ok (v2', bs2') <- loadDoc fsw did1
    assertEqual v2 v2'
    assertEqual bs2 bs2'
    Ok (v3', bs3') <- loadDoc fsw did2
    assertEqual v3 v3'
    assertEqual bs3 bs3'
    Fail _ <- loadDoc fsw did3
    docs <- listAllDocs fs
    assertEqual (HashSet.fromList [did1, did2]) docs
    deleteDocs fs [did1]
    Fail _ <- loadDoc fsw did1
    docs' <- listAllDocs fs
    assertEqual (HashSet.singleton did2) docs'
    v4 <- storeDoc fs did1 bs1 -- revive did1
    assertEqual (mkVersion 4) v4
    Ok (v4', bs4) <- loadDoc fsw did1
    assertEqual v4 v4'
    assertEqual bs1 bs4
    logNote "Test finished"
 where
  pollInterval = milliseconds 10
  did1 = DocId "doc1"
  did2 = DocId "doc2"
  did3 = DocId "doc3"
  bs1 = BSC.pack "foo"
  bs2 = BSC.pack "bar"
  bs3 = BSC.pack "spam"

test_waitChanges :: IO ()
test_waitChanges = do
  withSysTempDir $ \rootDir -> withFsAndWatcher rootDir pollInterval $ \(fs, fsw) -> do
    v1 <- storeDoc fs did1 bs1
    wait
    newObjs1 <- atomically $ waitForFileStoreChanges fsw
    assertEqual (FileStoreChanges (HashMap.singleton did1 v1) HashSet.empty) newObjs1
    _ <- storeDoc fs did1 bs1
    v2 <- storeDoc fs did1 bs2
    v3 <- storeDoc fs did2 bs3
    wait
    newObjs2 <- atomically $ waitForFileStoreChanges fsw
    assertEqual
      (FileStoreChanges (HashMap.fromList [(did1, v2), (did2, v3)]) HashSet.empty)
      newObjs2
    _ <- storeDoc fs did1 bs1
    v4 <- storeDoc fs did2 bs1
    deleteDocs fs [did1]
    wait
    newObjs3 <- atomically $ waitForFileStoreChanges fsw
    assertEqual (FileStoreChanges (HashMap.singleton did2 v4) (HashSet.singleton did1)) newObjs3
 where
  pollInterval = milliseconds 10
  wait = c_sleep realClock (2 * pollInterval)
  did1 = DocId "doc1"
  did2 = DocId "doc2"
  bs1 = BSC.pack "foo"
  bs2 = BSC.pack "bar"
  bs3 = BSC.pack "spam"

withFsAndWatcher :: FilePath -> TimeSpan -> ((FileStore, FileStoreWatcher) -> IO a) -> IO a
withFsAndWatcher rootDir pollInterval action =
  withFileStore (FileStoreCfg rootDir) $ \fs ->
    withFileStoreWatcher (FileStoreWatcherCfg rootDir pollInterval) $ \w -> action (fs, w)
