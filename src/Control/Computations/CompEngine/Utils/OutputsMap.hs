{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Utils.OutputsMap (
  -- * Map type
  OutputsMap,
  OutputsForwardMap,
  OutputsReverseMap,
  OutputKeyHash,

  -- * Query
  forwardMap,
  lookup,
  lookupOutputKey,

  -- * Construction
  empty,
  fromForwardMap,
  fromOrderedForwardMap,
  insert,
  insertWith,
  delete,

  -- * Other
  filterUnreferencedOutputs,

  -- * Tests
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompSink
import Control.Computations.Utils.StrictList (SL)
import qualified Control.Computations.Utils.StrictList as SL
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)
import Test.Framework
import qualified Test.QuickCheck as QC
import Prelude hiding (lookup)

-- This is a map connecting keys to outputs, which can be queried in both directions.
-- The 'om_forward' map provides a direct way to lookup all outputs for a key.
-- For the other direction, we use the hashes of outputs. The map under 'om_reverse' stores for
-- every hash of an output which keys have an (there may be multiple!) output with that hash value.
-- When querying for an output, we have to also do a lookup in the 'om_forward' map to filter out
-- keys that only have another output which by coincidence has the same hash value.

type OutputsForwardMap k = HashMap k AnyCompSinkOutsMap
type OutputsReverseMap k =
  IntMap (SL k)
  -- ^ the strict list must be in ascending order

type OutputKeyHash = Int

data OutputsMap k = OutputsMap
  { om_forward :: OutputsForwardMap k
  , om_reverse :: OutputsReverseMap k
  }
  deriving (Show)

forwardMap :: OutputsMap k -> OutputsForwardMap k
forwardMap = om_forward

lookup
  :: (Hashable k)
  => k
  -> OutputsMap k
  -> Maybe AnyCompSinkOutsMap
lookup key om =
  HashMap.lookup key (om_forward om)

-- | Look up the output represented by the triple (reverse lookup)
lookupOutputKey
  :: (CompSink s, Hashable k)
  => (Proxy s, CompSinkId, CompSinkOut s)
  -> OutputsMap k
  -> SL k
lookupOutputKey outputIdent@(_proxy, _dataIfKey, outputKey) om =
  SL.filter (hasOutput outputIdent om) $
    fromMaybe mempty $
      IntMap.lookup h (om_reverse om)
 where
  h = hash outputKey

empty :: OutputsMap k
empty =
  OutputsMap
    { om_forward = HashMap.empty
    , om_reverse = IntMap.empty
    }

mkSingletonReverseMap :: k -> AnyCompSinkOutsMap -> OutputsReverseMap k
mkSingletonReverseMap key (AnyCompSinkOutsMap outsMap) =
  IntMap.fromList $
    map (\hash -> (hash, SL.singleton key)) $
      Map.elems outsMap >>= hashAnyOutputs

outputKeyHash :: CompSink s => Proxy s -> CompSinkOut s -> OutputKeyHash
outputKeyHash _ = hash

hashAnyOutputs :: AnyCompSinkOuts -> [OutputKeyHash]
hashAnyOutputs (ForAnyCompFlow _ p outputsWithIdent) =
  map (outputKeyHash p) $ HashSet.toList $ unSomeCompSinkOut outputsWithIdent

-- | Helper function. Checks if the key has the output identified by the tripel.
hasOutput
  :: forall s k
   . (CompSink s, Hashable k)
  => (Proxy s, CompSinkId, CompSinkOut s)
  -> OutputsMap k
  -> k
  -> Bool
hasOutput (_, compSinkId, outputKey) om key =
  Just True
    == do
      AnyCompSinkOutsMap manyOutputs <- HashMap.lookup key (om_forward om)
      ForAnyCompFlow _ _ (SomeCompSinkOuts outputsForCompSink) <- Map.lookup compSinkId manyOutputs
      outputsForCompSink' <- gcast outputsForCompSink
      pure (HashSet.member outputKey (outputsForCompSink' :: CompSinkOuts s))

-- | Helper function. Checks if the key has an output with the given hash.
hasOutputByHash :: (Hashable k) => OutputsMap k -> OutputKeyHash -> k -> Bool
hasOutputByHash om h key =
  case HashMap.lookup key (om_forward om) of
    Nothing -> False
    Just outputs -> anyOutsMap (outputHasHash h) outputs
 where
  outputHasHash :: CompSink s => OutputKeyHash -> Proxy s -> CompSinkOut s -> Bool
  outputHasHash h _ output = hash output == h

fromForwardMapGeneric
  :: (Ord k, Hashable k)
  => (map k AnyCompSinkOutsMap -> [(k, AnyCompSinkOutsMap)])
  -> map k AnyCompSinkOutsMap
  -> OutputsMap k
fromForwardMapGeneric toKVPairs m =
  let pairs = toKVPairs m
   in OutputsMap
        { om_forward = HashMap.fromList pairs
        , om_reverse = IntMap.unionsWith SL.merge $ map (uncurry mkSingletonReverseMap) pairs
        }

fromForwardMap
  :: (Ord k, Hashable k)
  => OutputsForwardMap k
  -> OutputsMap k
fromForwardMap = fromForwardMapGeneric HashMap.toList

fromOrderedForwardMap
  :: (Ord k, Hashable k)
  => Map k AnyCompSinkOutsMap
  -> OutputsMap k
fromOrderedForwardMap = fromForwardMapGeneric Map.toList

{- | Internally used helper function. The given outputs are not "dirty", because their entry in the
 'om_reverse' map may not be up to date (specifically, the list of keys stored there may contain
 false positives). This function brings the map up to date.
-}
cleanReverse
  :: forall k
   . (Hashable k)
  => AnyCompSinkOutsMap
  -> OutputsMap k
  -> OutputsMap k
cleanReverse changedOutputs om =
  let hashes = mapAnyOutsMap outputKeyHash changedOutputs
   in om{om_reverse = foldl' (flip cleanReverseForHash) (om_reverse om) hashes}
 where
  cleanReverseForHash :: OutputKeyHash -> IntMap.IntMap (SL k) -> IntMap.IntMap (SL k)
  cleanReverseForHash h im =
    IntMap.adjust (SL.filter (hasOutputByHash om h)) h im

insertWith
  :: (Hashable k, Ord k)
  => (AnyCompSinkOutsMap -> AnyCompSinkOutsMap -> AnyCompSinkOutsMap)
  -> k
  -> AnyCompSinkOutsMap
  -> OutputsMap k
  -> OutputsMap k
insertWith combine key outputs om =
  let mOldOutputs = HashMap.lookup key (om_forward om)
      currentOutputs = maybe outputs (combine outputs) mOldOutputs
      deletedOutputs = maybe mempty (`diffAnyOutsMap` currentOutputs) mOldOutputs
   in cleanReverse deletedOutputs $
        OutputsMap
          { om_forward = HashMap.insert key currentOutputs (om_forward om)
          , om_reverse = IntMap.unionWith SL.merge (mkSingletonReverseMap key outputs) (om_reverse om)
          }

insert
  :: (Hashable k, Ord k)
  => k
  -> AnyCompSinkOutsMap
  -> OutputsMap k
  -> OutputsMap k
insert = insertWith (\newOutputs _oldOutputs -> newOutputs)

delete
  :: (Hashable k)
  => k
  -> OutputsMap k
  -> OutputsMap k
delete key om =
  OutputsMap
    { om_forward = HashMap.delete key (om_forward om)
    , om_reverse =
        let mOldOutputs = HashMap.lookup key (om_forward om)
            hashes = maybe [] (mapAnyOutsMap outputKeyHash) mOldOutputs
         in foldl' (\revMap h -> IntMap.adjust (SL.filter (/= key)) h revMap) (om_reverse om) hashes
    }

-- | Returns only the outputs which are *not* referenced (any more).
filterUnreferencedOutputs :: (Hashable k) => OutputsMap k -> AnyCompSinkOutsMap -> AnyCompSinkOutsMap
filterUnreferencedOutputs om = filterAnyOutsMap notReferenced
 where
  notReferenced :: CompSink s => Proxy s -> CompSinkId -> CompSinkOut s -> Bool
  notReferenced p k output =
    SL.null (lookupOutputKey (p, k, output) om)

--
--
--
newtype CompSinkForTestingOutputs = CompSinkForTestingOutputs
  { csfto_ident :: CompSinkInstanceId
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Hashable CompSinkForTestingOutputs

data CompSinkForTestingOutputsReq a

instance CompSink CompSinkForTestingOutputs where
  type CompSinkReq CompSinkForTestingOutputs = CompSinkForTestingOutputsReq
  type CompSinkOut CompSinkForTestingOutputs = T.Text
  compSinkInstanceId = csfto_ident
  compSinkExecute _ _ = fail "CompSinkForTestingOutputs actions do not exist"
  compSinkDeleteOutputs _ _ = pure ()
  compSinkListExistingOutputs _ = None

data ToyOutput = ToyOutput
  { to_dataIf :: CompSinkForTestingOutputs
  , to_key :: T.Text
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable ToyOutput

instance QC.Arbitrary ToyOutput where
  arbitrary = QC.elements allPossibleToyOutputs

type ToyForwardMap k = HashMap.HashMap k (HashSet ToyOutput)

-- Computationally inefficient but simple enough to be obviously correct implementation of
-- (a special case with simpler types of) an output map.
newtype ToyOutputsMap k = ToyOutputsMap
  { tom_forward :: ToyForwardMap k
  }
  deriving (Eq, Show)

toy_forwardMap :: ToyOutputsMap k -> ToyForwardMap k
toy_forwardMap = tom_forward

toy_lookup :: (Hashable k) => k -> ToyOutputsMap k -> Maybe (HashSet ToyOutput)
toy_lookup key = HashMap.lookup key . toy_forwardMap

toy_lookupOutputKey :: (Hashable k) => ToyOutput -> ToyOutputsMap k -> HashSet k
toy_lookupOutputKey outputKey tom =
  HashSet.fromList $
    map fst $
      filter (any (\output -> output == outputKey) . snd) $
        HashMap.toList $
          toy_forwardMap tom

toy_fromForwardMap :: ToyForwardMap k -> ToyOutputsMap k
toy_fromForwardMap m = ToyOutputsMap{tom_forward = m}

toy_insert :: (Hashable k) => k -> HashSet ToyOutput -> ToyOutputsMap k -> ToyOutputsMap k
toy_insert key outputs tom =
  ToyOutputsMap{tom_forward = HashMap.insert key outputs (tom_forward tom)}

toy_delete :: (Hashable k) => k -> ToyOutputsMap k -> ToyOutputsMap k
toy_delete key tom =
  ToyOutputsMap{tom_forward = HashMap.delete key (tom_forward tom)}

testOutputs :: CompSinkForTestingOutputs -> [T.Text] -> AnyCompSinkOuts
testOutputs sink outputs =
  let i = compSinkId sink
      outputSet :: SomeCompSinkOuts CompSinkForTestingOutputs
      outputSet = SomeCompSinkOuts (HashSet.fromList outputs)
   in ForAnyCompFlow i (Proxy @CompSinkForTestingOutputs) outputSet

toy_outputsToReal :: HashSet ToyOutput -> AnyCompSinkOutsMap
toy_outputsToReal toyOutputs =
  let toyOutputsMap =
        Map.fromListWith HashSet.union $
          map (\toyOutput -> (to_dataIf toyOutput, HashSet.singleton (to_key toyOutput))) $
            HashSet.toList toyOutputs
   in AnyCompSinkOutsMap $
        Map.fromList $
          map (\(dataIf, deps) -> (compSinkId dataIf, testOutputs dataIf (HashSet.toList deps))) $
            Map.toList toyOutputsMap

toy_outputsMapToReal :: (Ord k, Hashable k) => ToyOutputsMap k -> OutputsMap k
toy_outputsMapToReal =
  fromForwardMap . fmap toy_outputsToReal . toy_forwardMap

allPossibleToyOutputKeys :: [ToyOutput]
allPossibleToyOutputKeys =
  ToyOutput
    <$> allPossibleCompSinks
    <*> possibleOutputKeys
 where
  possibleOutputKeys =
    do
      prefix <- ["foo", "bar", "blub"]
      index <- [1 .. 4] :: [Int]
      pure $ prefix <> showText index

allPossibleToyOutputs :: [ToyOutput]
allPossibleToyOutputs = allPossibleToyOutputKeys

allPossibleCompSinks :: [CompSinkForTestingOutputs]
allPossibleCompSinks =
  map CompSinkForTestingOutputs ["foo", "bar"]

toyToRealOutputKey :: ToyOutput -> (Proxy CompSinkForTestingOutputs, CompSinkId, T.Text)
toyToRealOutputKey tok =
  (Proxy, compSinkId (to_dataIf tok), to_key tok)

newtype ToyKey = ToyKey
  { _unToyKey :: T.Text
  }
  deriving (Show, Eq, Ord, Hashable)

allPossibleToyKeys :: [ToyKey]
allPossibleToyKeys =
  map (\i -> ToyKey ("capKey" <> showText i)) ([1 .. 20] :: [Int])

instance QC.Arbitrary ToyKey where
  arbitrary =
    QC.elements allPossibleToyKeys

instance (QC.Arbitrary k, Hashable k) => QC.Arbitrary (ToyOutputsMap k) where
  arbitrary =
    toy_fromForwardMap . HashMap.fromList
      <$> QC.listOf (liftA2 (,) arbitrary genToyHashSet)
   where
    genToyHashSet =
      HashSet.fromList <$> QC.listOf (QC.elements allPossibleToyOutputs)
    liftA2 f x = (<*>) (fmap f x)

checkLookup :: (Hashable k) => ToyOutputsMap k -> OutputsMap k -> k -> QC.Property
checkLookup tom om key =
  (toy_outputsToReal <$> toy_lookup key tom) QC.=== lookup key om

checkLookupOutputKey
  :: (Show k, Ord k, Hashable k)
  => ToyOutputsMap k
  -> OutputsMap k
  -> ToyOutput
  -> QC.Property
checkLookupOutputKey tom om tok =
  let tomResult = toy_lookupOutputKey tok tom
      omResult = lookupOutputKey (toyToRealOutputKey tok) om
   in tomResult QC.=== HashSet.fromList (SL.toList omResult)

checkLookupAllKeys :: ToyOutputsMap ToyKey -> OutputsMap ToyKey -> QC.Property
checkLookupAllKeys tom om =
  QC.conjoin $ map (checkLookup tom om) allPossibleToyKeys

prop_lookup :: ToyOutputsMap ToyKey -> QC.Property
prop_lookup tom =
  checkLookupAllKeys tom (toy_outputsMapToReal tom)

checkLookupAllOutputKeys :: ToyOutputsMap ToyKey -> OutputsMap ToyKey -> QC.Property
checkLookupAllOutputKeys tom om =
  QC.conjoin $ map (checkLookupOutputKey tom om) allPossibleToyOutputKeys

prop_lookupOutputKey :: ToyOutputsMap ToyKey -> QC.Property
prop_lookupOutputKey tom =
  checkLookupAllOutputKeys tom (toy_outputsMapToReal tom)

prop_insert :: ToyKey -> [ToyOutput] -> ToyOutputsMap ToyKey -> QC.Property
prop_insert key outsList tom =
  let outsSet = HashSet.fromList outsList
      om = toy_outputsMapToReal tom
      realAnyCompSinkOutsMap = toy_outputsToReal outsSet
      omAfterInsert = insert key realAnyCompSinkOutsMap om
      tomAfterInsert = toy_insert key outsSet tom
   in (lookup key omAfterInsert QC.=== Just realAnyCompSinkOutsMap)
        QC..&. checkLookupAllOutputKeys tomAfterInsert omAfterInsert
        QC..&. checkLookupAllKeys tomAfterInsert omAfterInsert

prop_delete :: ToyKey -> ToyOutputsMap ToyKey -> QC.Property
prop_delete key tom =
  let om = toy_outputsMapToReal tom
      omAfterDelete = delete key om
      tomAfterDelete = toy_delete key tom
   in (lookup key omAfterDelete QC.=== Nothing)
        QC..&. checkLookupAllOutputKeys tomAfterDelete omAfterDelete
        QC..&. checkLookupAllKeys tomAfterDelete omAfterDelete
