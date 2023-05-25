{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.FlowImpls.FileSink (
  FileSinkReq (..),
  FileSink,
  FileSinkOut,
  makeFileSink,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.CompSink
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging
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
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import Test.Framework

{- | A request for writing a file or creating a directory. All paths are interpreted relatively to
 to root of the file sink. Using absolute paths is an error.
-}
data FileSinkReq a where
  WriteFile :: FilePath -> BS.ByteString -> FileSinkReq ()
  MakeDirs :: FilePath -> FileSinkReq ()

instance Show (FileSinkReq a) where
  showsPrec p req =
    showParen (p > 10) $
      case req of
        WriteFile p bs ->
          showString "WriteFile "
            . showString p
            . showString " <"
            . shows (BS.length bs)
            . showString " bytes>"
        MakeDirs p ->
          showString "MakeDirs "
            . showString p

data FileSink = FileSink
  { fcs_ident :: CompSinkInstanceId
  , fcs_root :: CanonPath
  }
  deriving (Show)

makeFileSink :: T.Text -> FilePath -> IO FileSink
makeFileSink i root' =
  do
    root <- canonPath root'
    pure (FileSink (CompSinkInstanceId i) root)

data FileOrDir = File | Dir
  deriving (Show, Eq, Generic, Hashable)

-- | A path relative to the root of the file sink.
newtype OutPath = OutPath FilePath
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

myNormalize :: FilePath -> FilePath
myNormalize fp =
  case normalise fp of
    '.' : '/' : rest -> rest
    x -> x

outPath :: FilePath -> OutPath
outPath = OutPath . myNormalize

mkOutPath :: FilePath -> IO OutPath
mkOutPath p =
  if isRelative p
    then pure (outPath p)
    else fail ("Absolute paths are not supported by FileSinks")

qualifyPath :: FileSink -> FilePath -> IO (FilePath, OutPath)
qualifyPath sink p =
  do
    out <- mkOutPath p
    pure (unCanonPath (fcs_root sink) </> p, out)

qualifyOutPath :: FileSink -> OutPath -> FilePath
qualifyOutPath sink (OutPath p) = unCanonPath (fcs_root sink) </> p

outPathJoin :: OutPath -> String -> OutPath
outPathJoin (OutPath p) s = outPath (p </> s)

data FileSinkOut = FileSinkOut
  { fcso_path :: OutPath -- inside root
  , fcso_type :: FileOrDir
  }
  deriving (Show, Eq, Generic, Hashable)

instance CompSink FileSink where
  type CompSinkReq FileSink = FileSinkReq
  type CompSinkOut FileSink = FileSinkOut
  compSinkInstanceId = fcs_ident
  compSinkExecute = executeImpl
  compSinkDeleteOutputs = deleteImpl
  compSinkListExistingOutputs s = Some (listExistingOutputsImpl s)

executeImpl :: FileSink -> FileSinkReq a -> IO (HashSet FileSinkOut, Fail a)
executeImpl sink req =
  do
    logDebug ("Executing request " ++ show req)
    case req of
      WriteFile p' bs ->
        do
          (p, out) <- qualifyPath sink p'
          -- if another entry exists at the same path, delete it
          -- no need to delete if the existing entry is a file because
          -- then we will overwrite it anyway
          s <- safeGetFileStatus p
          case fmap fs_type s of
            None -> pure ()
            Some RegularFileType -> pure ()
            _ -> remove p s
          res <- failForIOException $ BS.writeFile p bs
          logIfFailure res
          pure (HashSet.singleton (FileSinkOut out File), res)
      MakeDirs p' ->
        do
          (p, out) <- qualifyPath sink p'
          -- remove a potential exising entry
          remove p None
          res <- failForIOException $ createDirectoryIfMissing True p
          logIfFailure res
          pure (HashSet.singleton (FileSinkOut out Dir), res)
 where
  remove :: FilePath -> Option FileStatus -> IO ()
  remove p mStatus =
    do
      status <-
        case mStatus of
          None -> safeGetFileStatus p
          Some _ -> pure mStatus
      case fmap fs_type status of
        None -> pure () -- ok, does not exist
        Some DirectoryFileType ->
          removeDirectoryRecursive p
        Some _ ->
          removeFile p
      `catch` (\(e :: IOException) -> logDebug ("Error removing " ++ p ++ ": " ++ show e))
  logIfFailure :: Fail a -> IO ()
  logIfFailure res =
    case res of
      Fail msg -> logDebug ("Request " ++ show req ++ " failed: " ++ msg)
      Ok _ -> pure ()

deleteImpl :: FileSink -> HashSet FileSinkOut -> IO ()
deleteImpl sink outs = mapM_ del (HashSet.toList outs)
 where
  del out =
    do
      let p = qualifyOutPath sink (fcso_path out)
      s <- safeGetFileStatus p
      case (fcso_type out, fmap fs_type s) of
        (_, None) -> logDebug ("Path " ++ p ++ " no longer exists")
        (Dir, Some DirectoryFileType) ->
          do
            logDebug ("Deleting output directory " ++ p)
            removeDirectoryRecursive p
        (File, Some RegularFileType) ->
          do
            logDebug ("Deleting output file " ++ p)
            removeFile p
        (t, Some k) ->
          logDebug ("When deleting, ignoring path of kind " ++ show k ++ " (old type: " ++ show t ++ ")")
      `catch` (\(e :: IOException) -> logWarn ("Error deleting output " ++ show out ++ ": " ++ show e))

listExistingOutputsImpl :: FileSink -> IO (HashSet FileSinkOut)
listExistingOutputsImpl sink =
  -- don't include, otherwise it gets deleted if no computation explicitly creates it
  loop (outPath ".") HashSet.empty
 where
  loop outP acc =
    do
      let p = qualifyOutPath sink outP
      x <- failForIOException (listDirectory p)
      case x of
        Fail err ->
          do
            logInfo ("Ignoring " ++ p ++ " when listing existing outputs: " ++ err)
            pure acc
        Ok l -> foldM handle acc (map (outPathJoin outP) l)
  handle acc outP =
    do
      let p = qualifyOutPath sink outP
      s <- safeGetFileStatus p
      case fmap fs_type s of
        None ->
          do
            logInfo ("Cannot get file status of " ++ p)
            pure acc
        Some RegularFileType -> pure (HashSet.insert (FileSinkOut outP File) acc)
        Some DirectoryFileType ->
          if HashSet.member (FileSinkOut outP Dir) acc
            then pure acc -- cycle
            else loop outP (HashSet.insert (FileSinkOut outP Dir) acc)
        Some k ->
          do
            logDebug ("Ignoring path of kind " ++ show k)
            pure acc

test_basics :: IO ()
test_basics =
  withSysTempDir $ \rootDir ->
    do
      sink <- makeFileSink "ident" rootDir
      void $ executeImpl sink (WriteFile "x" "1")
      (out2, Ok ()) <- executeImpl sink (MakeDirs "d1/d2")
      void $ executeImpl sink (WriteFile "d1/d2/y" "2")
      subAssert $
        assertContent
          sink
          [("x", File), ("d1", Dir), ("d1/d2", Dir), ("d1/d2/y", File)]
          [("x", "1"), ("d1/d2/y", "2")]
      deleteImpl sink out2
      deleteImpl sink out2 -- no error
      subAssert $
        assertContent
          sink
          [("x", File), ("d1", Dir)]
          [("x", "1")]
      void $ executeImpl sink (WriteFile "d1" "3")
      subAssert $
        assertContent
          sink
          [("x", File), ("d1", File)]
          [("x", "1"), ("d1", "3")]
      void $ executeImpl sink (MakeDirs "x")
      subAssert $
        assertContent
          sink
          [("x", Dir), ("d1", File)]
          [("d1", "3")]
 where
  assertContent :: FileSink -> [(FilePath, FileOrDir)] -> [(FilePath, BS.ByteString)] -> IO ()
  assertContent sink outs fileContents =
    do
      realOuts <- listExistingOutputsImpl sink
      outs' <- forM outs $ \(p, t) ->
        do
          outP <- mkOutPath p
          pure (FileSinkOut outP t)
      assertEqual (HashSet.fromList outs') realOuts
      subAssert $ forM_ fileContents (assertFileContent sink)
  assertFileContent :: FileSink -> (FilePath, BS.ByteString) -> IO ()
  assertFileContent sink (p, c) =
    do
      let fullP = unCanonPath (fcs_root sink) </> p
      bs <- BS.readFile fullP
      assertEqual c bs
