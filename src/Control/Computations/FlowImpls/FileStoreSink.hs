{-# LANGUAGE TypeFamilies #-}

module Control.Computations.FlowImpls.FileStoreSink (
  FileStoreSinkReq (..),
  FileStoreSink,
  withFileStoreSink,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.CompSink
import Control.Computations.Utils.FileStore.Writer
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.ByteString as BS
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T

data FileStoreSinkReq a where
  WriteDoc :: DocId -> BS.ByteString -> FileStoreSinkReq ObjId

data FileStoreSink = FileStoreSink
  { fss_ident :: CompSinkInstanceId
  , fss_store :: FileStore
  }

withFileStoreSink :: T.Text -> FileStoreCfg -> (FileStoreSink -> IO a) -> IO a
withFileStoreSink i cfg action =
  withFileStore cfg $ \fs -> action (FileStoreSink (CompSinkInstanceId i) fs)

instance CompSink FileStoreSink where
  type CompSinkReq FileStoreSink = FileStoreSinkReq
  type CompSinkOut FileStoreSink = DocId
  compSinkInstanceId = fss_ident
  compSinkExecute = executeImpl
  compSinkDeleteOutputs = deleteImpl
  compSinkListExistingOutputs s = Some (listExistingOutputsImpl s)

executeImpl :: FileStoreSink -> FileStoreSinkReq a -> IO (HashSet DocId, Fail a)
executeImpl sink (WriteDoc did bs) = do
  v <- storeDoc (fss_store sink) did bs
  let objId = ObjId did v
  pure (HashSet.singleton did, Ok objId)

deleteImpl :: FileStoreSink -> HashSet DocId -> IO ()
deleteImpl sink outs = deleteDocs (fss_store sink) (HashSet.toList outs)

listExistingOutputsImpl :: FileStoreSink -> IO (HashSet DocId)
listExistingOutputsImpl sink = listAllDocs (fss_store sink)
