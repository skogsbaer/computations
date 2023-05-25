{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.SifCache (
  SifCache,
  CompSize (..),
  empty,
  null,
  insert,
  delete,
  lookup,
  size,
  toMap,
  fromMap,
  toList,
  totalInstanceCount,
  dataSizeForCompId,
  compIdSizeMap,
  compIdSizeList,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.DataSize
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad.Identity
import Data.Int
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Framework
import Prelude hiding (lookup, null)

data AbstractSifCache a = AbstractSifCache
  { sifc_keyToVal :: Map AnyCompAp a
  , sifc_compToSize :: Map CompId CompSize
  }
  deriving (Show, Eq)

data CompSize = CompSize
  { cs_dataSize :: Option DataSize
  , cs_size :: Option Int
  , cs_instanceCount :: Int
  }
  deriving (Show, Eq)

instance Semigroup CompSize where
  (<>) = csPlus

instance Monoid CompSize where
  mempty = CompSize (Some 0) (Some 0) 0

csPlus :: CompSize -> CompSize -> CompSize
csPlus (CompSize mds1 ms1 c1) (CompSize mds2 ms2 c2) =
  let cs_dataSize =
        case (mds1, mds2) of
          (Some ds1, Some ds2) -> Some (ds1 + ds2)
          _ -> None
      cs_instanceCount = c1 + c2
      cs_size =
        case (ms1, ms2) of
          (Some s1, Some s2) -> Some (s1 + s2)
          _ -> None
   in CompSize{..}

csMinus :: CompSize -> CompSize -> CompSize
csMinus (CompSize mds1 ms1 c1) (CompSize mds2 ms2 c2) =
  let cs_dataSize =
        case (mds1, mds2) of
          (Some ds1, Some ds2) -> Some (ds1 - ds2)
          _ -> None
      cs_instanceCount = c1 - c2
      cs_size =
        case (ms1, ms2) of
          (Some s1, Some s2) -> Some (s1 - s2)
          _ -> None
   in CompSize{..}

compSize :: HasSizes a => a -> CompSize
compSize x =
  CompSize
    { cs_dataSize = dataSize x
    , cs_size = intSize x
    , cs_instanceCount = 1
    }

type SifCache = AbstractSifCache (CapResult AnyCompCacheValue)

class HasSizes a where
  dataSize :: a -> Option DataSize
  intSize :: a -> Option Int

instance HasSizes (CapResult AnyCompCacheValue) where
  dataSize = fromCapResult (Some 0) . fmap (anyCompCacheValueApply ccv_cacheSize)
  intSize = fromCapResult None . fmap (anyCompCacheValueApply $ ccm_cachedSize . ccv_meta)

empty :: AbstractSifCache a
empty = AbstractSifCache Map.empty Map.empty

size :: AbstractSifCache a -> Int
size = Map.size . sifc_keyToVal

null :: AbstractSifCache a -> Bool
null = Map.null . sifc_keyToVal

insert :: (HasSizes a) => AnyCompAp -> a -> AbstractSifCache a -> AbstractSifCache a
insert key@(AnyCompAp cap) val cache@(AbstractSifCache keyToVal compToSize) =
  AbstractSifCache
    { sifc_keyToVal = Map.insert key val keyToVal
    , sifc_compToSize = Map.insert compId newCompSize compToSize
    }
 where
  newCompSize = oldCompIdSize `csPlus` newEntrySize `csMinus` oldEntrySize
  compId = capCompId cap
  oldCompIdSize = dataSizeForCompId compId cache
  oldEntrySize = maybe mempty compSize $ Map.lookup key keyToVal
  newEntrySize = compSize val

delete :: HasSizes a => AnyCompAp -> AbstractSifCache a -> AbstractSifCache a
delete key@(AnyCompAp cap) cache@(AbstractSifCache keyToVal compToSize) =
  AbstractSifCache
    { sifc_keyToVal = Map.delete key keyToVal
    , sifc_compToSize =
        if oldCompIdSize == oldEntrySize
          then Map.delete compId compToSize
          else Map.insert compId (oldCompIdSize `csMinus` oldEntrySize) compToSize
    }
 where
  compId = capCompId cap
  oldCompIdSize = dataSizeForCompId compId cache
  oldEntrySize = maybe mempty compSize $ Map.lookup key keyToVal

lookup :: AnyCompAp -> AbstractSifCache a -> Maybe (AnyCompAp, a)
lookup key cache =
  -- NB: returns the key used in the map. This is super important
  -- for sharing, SimpleStateIf relies in this.
  case Map.lookupLE key (sifc_keyToVal cache) of
    res@(Just (key', _)) | key' == key -> res
    _ -> Nothing

toMap :: AbstractSifCache a -> Map AnyCompAp a
toMap = sifc_keyToVal

fromMap :: HasSizes a => Map AnyCompAp a -> AbstractSifCache a
fromMap keyToVal =
  AbstractSifCache
    { sifc_keyToVal = keyToVal
    , sifc_compToSize = foldl' f Map.empty $ Map.toList keyToVal
    }
 where
  f compToSize (AnyCompAp cap, val) =
    let compId = capCompId cap
        oldSize = Map.findWithDefault mempty compId compToSize
     in Map.insert compId (oldSize `csPlus` compSize val) compToSize

toList :: AbstractSifCache a -> [(AnyCompAp, a)]
toList = Map.toList . sifc_keyToVal

compIdSizeMap :: AbstractSifCache a -> Map CompId CompSize
compIdSizeMap = sifc_compToSize

compIdSizeList :: AbstractSifCache a -> [(CompId, CompSize)]
compIdSizeList = Map.toList . compIdSizeMap

totalInstanceCount :: AbstractSifCache a -> Int
totalInstanceCount = foldl' (+) 0 . map (cs_instanceCount . snd) . Map.toList . sifc_compToSize

dataSizeForCompId :: CompId -> AbstractSifCache a -> CompSize
dataSizeForCompId compId = Map.findWithDefault mempty compId . sifc_compToSize

instance HasSizes Int where
  dataSize = Some . bytes
  intSize = Some

test_fromMap :: IO ()
test_fromMap = assertEqual actual desired
 where
  keyToVal =
    Map.fromList
      [ (mkAnyCompAp "c1" "a", 1 :: Int)
      , (mkAnyCompAp "c1" "b", 2)
      , (mkAnyCompAp "c2" "c", 5)
      ]
  actual = fromMap keyToVal
  desired =
    AbstractSifCache
      keyToVal
      ( Map.fromList
          [ ("c1", CompSize (Some $ DataSize 3) (Some 3) 2)
          , ("c2", CompSize (Some $ DataSize 5) (Some 5) 1)
          ]
      )

test_insert :: IO ()
test_insert = assertEqual actual desired
 where
  keyValuePairs =
    [ (mkAnyCompAp "c1" "a", 1 :: Int)
    , (mkAnyCompAp "c1" "b", 2)
    , (mkAnyCompAp "c2" "c", 5)
    ]
  actual = foldl' (\c (key, val) -> insert key val c) empty keyValuePairs
  desired =
    AbstractSifCache
      (Map.fromList keyValuePairs)
      ( Map.fromList
          [ ("c1", CompSize (Some $ DataSize 3) (Some 3) 2)
          , ("c2", CompSize (Some $ DataSize 5) (Some 5) 1)
          ]
      )

test_delete :: IO ()
test_delete = assertEqual actual desired
 where
  initial =
    fromMap
      ( Map.fromList
          [ (mkAnyCompAp "c1" "a", 1 :: Int)
          , (mkAnyCompAp "c1" "b", 2)
          , (mkAnyCompAp "c2" "c", 5)
          , (mkAnyCompAp "c3" "c", 11)
          ]
      )
  deleteKeys =
    [(mkAnyCompAp "c1" "b"), (mkAnyCompAp "c3" "c")]
  actual = foldl' (flip delete) initial deleteKeys
  keyToVal =
    Map.fromList
      [ (mkAnyCompAp "c1" "a", 1 :: Int)
      , (mkAnyCompAp "c2" "c", 5)
      ]
  desired =
    AbstractSifCache
      keyToVal
      ( Map.fromList
          [ ("c1", CompSize (Some $ DataSize 1) (Some 1) 1)
          , ("c2", CompSize (Some $ DataSize 5) (Some 5) 1)
          ]
      )

unitCaching :: CompCacheBehavior ()
unitCaching =
  CompCacheBehavior
    { ccb_memcache = \() ->
        CompCacheValue
          { ccv_payload = Some ()
          , ccv_meta =
              CompCacheMeta
                { ccm_largeHash = largeHash128 ()
                , ccm_logrepr = "()"
                , ccm_approxCachedSize = Some (bytes (16 :: Int))
                , ccm_cachedSize = Some 1
                }
          }
    }

mkAnyCompAp :: String -> String -> AnyCompAp
mkAnyCompAp compName param =
  AnyCompAp (mkCompAp comp param)
 where
  comp :: Comp String ()
  comp = Comp (mkCompId compName) unitCaching (\_ -> return ()) mempty
