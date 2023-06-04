{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Computations.Utils.IOUtils (
  listDirectoryWithQualifiedNames,
  listDirectoryRecursive,
  withSysTempDir,
  FileStatus (..),
  FileType (..),
  getFileStatus,
  safeGetFileStatus,
  CanonPath,
  unCanonPath,
  canonPath,
  writeFileAtomically,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import Data.Maybe
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import System.Directory hiding (canonicalizePath)
import qualified System.Directory as Dir
import System.FilePath
import System.IO.Temp
import qualified System.Posix as Posix
import qualified System.Posix.Types as PosixTypes
import Test.Framework

listDirectoryWithQualifiedNames :: FilePath -> IO [FilePath]
listDirectoryWithQualifiedNames path = do
  content <- listDirectory path
  pure (map (path </>) content)

type ListRecAcc = (HashSet Posix.FileID, [(FilePath, FileStatus)])

listDirectoryRecursive :: FilePath -> IO [(FilePath, FileStatus)]
listDirectoryRecursive path =
  do
    (_, l) <- loop (HashSet.empty, []) path
    pure (reverse l)
 where
  loop acc path =
    do
      content <- listDirectory path
      foldM (handle path) acc content
  handle :: FilePath -> ListRecAcc -> String -> IO ListRecAcc
  handle dir (inodes, content) name =
    do
      let p = dir </> name
      stat <- getFileStatus p
      let newAcc = (HashSet.insert (fs_inode stat) inodes, (p, stat) : content)
      case fs_type stat of
        DirectoryFileType ->
          if (fs_inode stat `HashSet.member` inodes)
            then pure newAcc
            else loop newAcc p
        _ -> pure newAcc

test_listDirectoryRecursive :: IO ()
test_listDirectoryRecursive =
  withSysTempDir $ \rootDir ->
    do
      BS.writeFile (rootDir </> "x") "foo"
      createDirectory (rootDir </> "A")
      BS.writeFile (rootDir </> "A/x") "bar"
      createDirectory (rootDir </> "A/B")
      BS.writeFile (rootDir </> "A/B/x") "baz"
      createDirectoryLink
        (rootDir </> "A") -- target
        (rootDir </> "A/B/c") -- name of link
      createFileLink
        (rootDir </> "x") -- target
        (rootDir </> "A/B/y") -- name of link
      l <- listDirectoryRecursive rootDir
      assertListsEqualAsSets
        (map (rootDir </>) ["x", "A", "A/x", "A/B", "A/B/x", "A/B/c", "A/B/y"])
        (map fst l)

withSysTempDir :: (FilePath -> IO a) -> IO a
withSysTempDir = withTempDirectory "/tmp" "IncComp_"

instance Hashable PosixTypes.CIno where
  hashWithSalt s (PosixTypes.CIno w64) = hashWithSalt s w64

instance Hashable PosixTypes.COff where
  hashWithSalt s (PosixTypes.COff i64) = hashWithSalt s i64

data FileStatus = FileStatus
  { fs_inode :: Posix.FileID
  , fs_size :: Posix.FileOffset
  , fs_mtime :: POSIXTime
  , fs_ctime :: POSIXTime
  , fs_type :: FileType
  -- no access time because we use FileStatus for tracking changes
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

data FileType
  = SocketFileType
  | NamedPipeFileType
  | SymbolicLinkFileType
  | BlockDeviceFileType
  | CharDeviceFileType
  | DirectoryFileType
  | RegularFileType
  | UnknownFileType
  deriving (Eq, Ord, Show, Generic, Hashable)

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  s <- Posix.getFileStatus path
  pure $
    FileStatus
      { fs_inode = Posix.fileID s
      , fs_size = Posix.fileSize s
      , fs_mtime = Posix.modificationTimeHiRes s
      , fs_ctime = Posix.statusChangeTimeHiRes s
      , fs_type = getFileType s
      }
 where
  getFileType s =
    fromMaybe UnknownFileType $
      lookup
        True
        [ (Posix.isBlockDevice s, BlockDeviceFileType)
        , (Posix.isCharacterDevice s, CharDeviceFileType)
        , (Posix.isNamedPipe s, NamedPipeFileType)
        , (Posix.isRegularFile s, RegularFileType)
        , (Posix.isDirectory s, DirectoryFileType)
        , (Posix.isSymbolicLink s, SymbolicLinkFileType)
        , (Posix.isSocket s, SocketFileType)
        ]

safeGetFileStatus :: FilePath -> IO (Option FileStatus)
safeGetFileStatus path =
  do
    stat <- getFileStatus path
    pure (Some stat)
    `catch` \(_ :: IOException) -> pure None

-- | An absolute path in canonical form.
newtype CanonPath = CanonPath FilePath
  deriving stock (Show)
  deriving newtype (Hashable, Eq, Ord, LH.LargeHashable)

unCanonPath :: CanonPath -> FilePath
unCanonPath (CanonPath p) = p

canonPath :: FilePath -> IO CanonPath
canonPath p =
  do
    cp <- Dir.canonicalizePath p
    pure (CanonPath cp)

writeFileAtomically :: FilePath -> BS.ByteString -> IO ()
writeFileAtomically p bs =
  withTempFile (takeDirectory p) "IncComps" $ \tmp h -> do
    BS.hPutStr h bs
    renameFile tmp p
