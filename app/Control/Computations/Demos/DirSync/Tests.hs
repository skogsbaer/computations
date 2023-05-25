{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Demos.DirSync.Tests (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Demos.DirSync.Main
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.List as L
import System.Directory
import System.FilePath
import Test.Framework
import Prelude hiding (readFile, writeFile)

data DirEntry
  = Dir FilePath
  | File FilePath BS.ByteString
  deriving (Eq, Show)

fmapPath :: DirEntry -> (FilePath -> FilePath) -> DirEntry
fmapPath (Dir p) f = Dir (f p)
fmapPath (File p c) f = File (f p) c

-- - Creates a directory d with file d/x as content.
-- - Then it renames d and creates a file with name d.
test_reviveDirAsFile :: IO ()
test_reviveDirAsFile = withLogLevel DEBUG $
  withSysTempDir $ \rootDir ->
    do
      let dirSrc = rootDir </> "src"
      let dirTgt = rootDir </> "target"
      createDirectory dirSrc
      -- start sync
      runVar <- newTVarIO None
      engineAsync <- async (syncDirs' runVar dirSrc dirTgt)
      run1 <- waitForRun runVar 1
      subAssert $ assertIsSynced dirSrc dirTgt
      -- first modifications
      createDirectory (dirSrc </> "d")
      BS.writeFile (dirSrc </> "d/x") "foo"
      -- wait for changes to propagate
      run2 <- waitForRun runVar (run1 + 1)
      subAssert $ assertIsSynced dirSrc dirTgt
      -- perform changes
      renamePath (dirSrc </> "d") (dirSrc </> "x")
      BS.writeFile (dirSrc </> "d") "bar"
      -- wait for changes to propagate
      _ <- waitForRun runVar (run2 + 1)
      subAssert $ assertIsSynced dirSrc dirTgt
      cancel engineAsync

waitForRun :: TVar (Option RunStats) -> Int -> IO Int
waitForRun runVar n = atomically $
  do
    mStats <- readTVar runVar
    case mStats of
      None -> retry
      Some stats
        | (rs_run stats < n || rs_staleCaps stats > 0) -> retry
        | otherwise -> pure (rs_run stats)

assertIsSynced :: FilePath -> FilePath -> IO ()
assertIsSynced src tgt =
  do
    wanted <- listEntries src
    existing <- listEntries tgt
    assertListsEqualAsSets wanted existing
 where
  listEntries :: FilePath -> IO [DirEntry]
  listEntries dir =
    do
      l <- listDirectoryRecursive dir
      l2 <- mapM mkEntry l
      pure (map (\e -> fmapPath e (removePrefix dir)) l2)
  removePrefix :: FilePath -> FilePath -> FilePath
  removePrefix dir p =
    if dir `L.isPrefixOf` p
      then case drop (length dir) p of
        '/' : rest -> rest
        x -> x
      else p
  mkEntry (fp, status) =
    do
      case fs_type status of
        DirectoryFileType -> pure (Dir fp)
        _ ->
          do
            bs <- BS.readFile fp
            pure (File fp bs)
