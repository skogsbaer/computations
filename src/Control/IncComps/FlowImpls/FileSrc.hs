{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.IncComps.FlowImpls.FileSrc (
  FileSrcReq (..),
  FileSrcConfig (..),
  defaultFileSrcConfig,
  DirEntry (..),
  FileSrc,
  initFileSrc,
  closeFileSrc,
  withFileSrc,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.Utils.Clock
import Control.IncComps.Utils.FileWatch
import Control.IncComps.Utils.IOUtils
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.TimeSpan
import Control.IncComps.Utils.TimeUtils
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import Test.Framework

data FileSrcReq a where
  ReadFile :: FilePath -> FileSrcReq BS.ByteString
  ListDir :: FilePath -> FileSrcReq (HashSet DirEntry)

data DirEntry = DirEntry
  { de_name :: String
  , de_type :: FileType
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

data FileSrcConfig = FileSrcConfig
  { fcsc_ident :: CompSrcInstanceId
  , fcsc_pollInterval :: TimeSpan
  , fcsc_clock :: Clock
  }

defaultFileSrcConfig :: T.Text -> FileSrcConfig
defaultFileSrcConfig i =
  FileSrcConfig
    { fcsc_ident = CompSrcInstanceId i
    , fcsc_pollInterval = milliseconds 100
    , fcsc_clock = realClock
    }

data FileSrc = FileSrc
  { fcs_config :: FileSrcConfig
  , fcs_fileWatch :: FileWatch
  }

initFileSrc :: FileSrcConfig -> IO FileSrc
initFileSrc cfg = do
  watch <- initFileWatch (fcsc_clock cfg) (fcsc_pollInterval cfg)
  pure (FileSrc cfg watch)

closeFileSrc :: FileSrc -> IO ()
closeFileSrc fcs = do
  closeFileWatch (fcs_fileWatch fcs)

withFileSrc :: FileSrcConfig -> (FileSrc -> IO a) -> IO a
withFileSrc cfg =
  bracket (initFileSrc cfg) closeFileSrc

newtype FileKey = FileKey {unFileKey :: CanonPath}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, LH.LargeHashable)

newtype FileVer = FileVer {unFileVer :: POSIXTime}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, LH.LargeHashable)

type FileDep = Dep FileKey (Option FileVer)

instance CompSrc FileSrc where
  type CompSrcReq FileSrc = FileSrcReq
  type CompSrcKey FileSrc = FileKey
  type CompSrcVer FileSrc = Option FileVer
  compSrcInstanceId = fcsc_ident . fcs_config
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

fileVerFromStatus :: FileStatus -> FileVer
fileVerFromStatus = FileVer . fs_mtime

waitChangesImpl :: FileSrc -> STM (HashSet FileDep)
waitChangesImpl fcs = do
  changes <- waitForFileChanges (fcs_fileWatch fcs)
  pure (HashSet.map mkInput changes)
 where
  mkInput :: FileChange -> FileDep
  mkInput c = Dep (FileKey (fc_path c)) (fmap fileVerFromStatus (fc_new c))

executeImpl
  :: forall a
   . FileSrc
  -> FileSrcReq a
  -> IO (HashSet FileDep, Fail a)
executeImpl fcs req =
  case req of
    ReadFile path -> doWork path BS.readFile
    ListDir path -> doWork path $ \p -> do
      l <- listDirectory p
      l2 <- forM l $ \name ->
        do
          s <- getFileStatus (path </> name)
          pure (DirEntry name (fs_type s))
      pure (HashSet.fromList l2)
 where
  doWork :: FilePath -> (FilePath -> IO a) -> IO (HashSet FileDep, Fail a)
  doWork path getResult = do
    canonP <- watchFile (fcs_fileWatch fcs) path
    let action = do
          res <- getResult path
          status <- getFileStatus path
          pure (Ok res, Some (fileVerFromStatus status))
    (res, version) <- catch action $ \(e :: IOException) -> pure (Fail (show e), None)
    let deps = HashSet.singleton (Dep (FileKey canonP) version)
    pure (deps, res)

unregisterImpl :: FileSrc -> HashSet FileKey -> IO ()
unregisterImpl fcs keys = do
  forM_ (HashSet.toList keys) $ \k -> unwatchFile (fcs_fileWatch fcs) (unFileKey k)

--
-- Tests
--
testConfig :: FileSrcConfig
testConfig =
  FileSrcConfig
    { fcsc_ident = "TestFileSrc"
    , fcsc_clock = realClock
    , fcsc_pollInterval = milliseconds 10
    }

test_getNotificationForChangedModTime :: IO ()
test_getNotificationForChangedModTime =
  withSysTempDir $ \rootDir' ->
    withFileSrc testConfig $ \fcsf -> do
      rootDir <- canonPath rootDir'
      testPath <- canonPath (unCanonPath rootDir </> "testfile")
      (deps1, result1) <- compSrcExecute fcsf (ReadFile (unCanonPath testPath))
      assertEqual [Dep (FileKey testPath) None] (HashSet.toList deps1)
      assertBool (isFail result1)

      BS.writeFile (unCanonPath testPath) "123"
      let nextChanges = do
            logDebug "Waiting for changes..."
            changes <- atomically (compSrcWaitChanges fcsf)
            logDebug ("Got changes: " ++ show changes)
            if HashSet.null changes
              then nextChanges
              else pure changes
      changes <- nextChanges
      [Dep (FileKey p1) (Some (FileVer m1))] <- pure (HashSet.toList changes)
      assertEqual testPath p1

      (deps2, result2) <- compSrcExecute fcsf (ReadFile (unCanonPath testPath))
      [Dep (FileKey p2) (Some (FileVer m2))] <- pure (HashSet.toList deps2)
      assertEqual testPath p2
      assertEqual m1 m2
      assertEqual (Ok (BSC.pack "123")) result2

      (deps3, Ok result3) <- compSrcExecute fcsf (ListDir (unCanonPath rootDir))
      [Dep (FileKey p3) (Some (FileVer m3))] <- pure (HashSet.toList deps3)
      assertEqual rootDir p3
      [actual3] <- pure (HashSet.toList result3)
      assertEqual "testfile" (de_name actual3)
      assertEqual RegularFileType (de_type actual3)

      testPath2 <- canonPath (unCanonPath rootDir </> "testfile2")
      BS.writeFile (unCanonPath testPath2) "xyz"
      (deps4, Ok result4) <- compSrcExecute fcsf (ListDir (unCanonPath rootDir))
      [Dep (FileKey p4) (Some (FileVer m4))] <- pure (HashSet.toList deps4)
      subAssert $ assertBefore m3 m4
      assertEqual rootDir p4
      assertEqual
        (HashSet.fromList ["testfile", "testfile2"])
        (HashSet.map de_name result4)

      BS.writeFile (unCanonPath testPath2) "abc"
      (deps5, result5) <- compSrcExecute fcsf (ReadFile (unCanonPath testPath2))
      [Dep (FileKey p5) (Some (FileVer m5))] <- pure (HashSet.toList deps5)
      assertEqual testPath2 p5
      subAssert $ assertBefore m2 m5
      assertEqual (Ok (BSC.pack "abc")) result5
 where
  assertBefore t1 t2 = do
    assertBoolVerbose
      ( "Timestamp "
          ++ formatUTCTimeHiRes' (posixSecondsToUTCTime t1)
          ++ " is not before timestamp "
          ++ formatUTCTimeHiRes' (posixSecondsToUTCTime t2)
      )
      (t1 < t2)
