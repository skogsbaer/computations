{-# LANGUAGE TypeFamilies #-}

-- | `CompSrc` and `CompSink` for testing.
module Control.IncComps.FlowImpls.HashMapFlow (
  HashMapFlow,
  HashMapReadReq (..),
  HashMapWriteReq (..),
  initHashMapFlow,
  Key,
  Val,
  Ver,

  -- * For testing
  hmfLookup,
  hmfInsert,
  getHashMap,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.CompSink
import Control.IncComps.CompEngine.CompSrc
import Control.IncComps.Utils.Hash
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Data.Typeable

type Key = BS.ByteString
type Val = BS.ByteString
type Ver = Hash128

type HashMapDep = Dep Key (Maybe Ver)

data HashMapFlow = HashMapFlow
  { hmf_ident :: T.Text
  , hmf_hashMapVar :: TVar (HashMap Key Val)
  , hmf_changesVar :: TVar (HashSet HashMapDep)
  }
  deriving (Typeable)

data HashMapReadReq a where
  HashMapLookupReq :: Key -> HashMapReadReq (Maybe Val)

instance CompSrc HashMapFlow where
  type CompSrcReq HashMapFlow = HashMapReadReq
  type CompSrcKey HashMapFlow = Key
  type CompSrcVer HashMapFlow = Maybe Ver
  compSrcInstanceId = CompSrcInstanceId . hmf_ident
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

initHashMapFlow :: T.Text -> IO HashMapFlow
initHashMapFlow ident = do
  hmV <- newTVarIO HashMap.empty
  setV <- newTVarIO HashSet.empty
  pure (HashMapFlow ident hmV setV)

waitChangesImpl :: HashMapFlow -> STM (HashSet HashMapDep)
waitChangesImpl hmf = do
  set <- readTVar (hmf_changesVar hmf)
  writeTVar (hmf_changesVar hmf) HashSet.empty
  if HashSet.null set
    then do
      logDebugSTM ("no changes in HashMapFlow " ++ T.unpack (hmf_ident hmf))
      retry
    else do
      logDebugSTM
        ( show (HashSet.size set)
            ++ " changes in HashMapFlow "
            ++ T.unpack (hmf_ident hmf)
        )
      pure set

executeImpl :: HashMapFlow -> HashMapReadReq a -> IO (HashSet HashMapDep, Fail a)
executeImpl hmf (HashMapLookupReq key) =
  do
    mVal <- hmfLookup hmf key
    return (HashSet.singleton (Dep key (fmap largeHash128 mVal)), Ok mVal)

hmfLookup :: HashMapFlow -> Key -> IO (Maybe Val)
hmfLookup hmf key = do
  m <- readTVarIO (hmf_hashMapVar hmf)
  pure (HashMap.lookup key m)

unregisterImpl :: HashMapFlow -> HashSet Key -> IO ()
unregisterImpl _ _ = pure ()

data HashMapWriteReq a where
  HashMapStoreReq :: Key -> Val -> HashMapWriteReq ()

instance CompSink HashMapFlow where
  type CompSinkReq HashMapFlow = HashMapWriteReq
  type CompSinkOut HashMapFlow = Key
  compSinkInstanceId = CompSinkInstanceId . hmf_ident
  compSinkExecute = executeWriteImpl
  compSinkDeleteOutputs = deleteOutputsImpl

  -- not all sinks support listing the existing outputs
  compSinkListExistingOutputs _ = None

executeWriteImpl :: HashMapFlow -> HashMapWriteReq a -> IO (HashSet Key, Fail a)
executeWriteImpl hmf (HashMapStoreReq k v) = do
  hmfInsert hmf k v
  return (HashSet.singleton k, Ok ())

hmfInsert :: HashMapFlow -> Key -> Val -> IO ()
hmfInsert hmf key val = do
  logDebug ("HashMapFlow: Inserting " ++ show key ++ " -> " ++ show val)
  atomically $ do
    modifyTVar' (hmf_hashMapVar hmf) (HashMap.insert key val)
    modifyTVar'
      (hmf_changesVar hmf)
      (HashSet.union (HashSet.singleton (Dep key (Just (largeHash128 val)))))

deleteOutputsImpl :: HashMapFlow -> HashSet Key -> IO ()
deleteOutputsImpl hmf keys = do
  logDebug ("HashMapFlow: Deleting " ++ show keys)
  atomically (modifyTVar' (hmf_hashMapVar hmf) $ \hm -> F.foldl' (flip HashMap.delete) hm keys)

getHashMap :: HashMapFlow -> IO (HashMap Key Val)
getHashMap hmdi =
  readTVarIO (hmf_hashMapVar hmdi)
