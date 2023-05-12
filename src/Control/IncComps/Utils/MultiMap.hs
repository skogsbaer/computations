{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- |
Module      : Mgw.Util.MultiMap
Description : Map keys to multiple values.  Each key value pair is contained at most once.
-}
module Control.IncComps.Utils.MultiMap (
  MultiMap,
  delete,
  elems,
  empty,
  filter,
  filterWithKey,
  fromList,
  toList,
  htf_thisModulesTests,
  insert,
  keys,
  lookup,
  union,
  numberOfKeys,
  numberOfKeyValuePairs,
  toSetList,
  fromSetList,
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import qualified Control.IncComps.Utils.MultiSet as MSet

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.List as L
import Test.Framework
import Prelude hiding (filter, lookup)

newtype MultiMap k v = MultiMap (HashMap k (HashSet v))
  deriving (Eq)

instance
  (Hashable k, Hashable v, Arbitrary k, Arbitrary v)
  => Arbitrary (MultiMap k v)
  where
  arbitrary = fromList <$> arbitrary

instance (Show k, Show v) => Show (MultiMap k v) where
  show x = show $ "MultiMap.fromList " ++ show (toList x)

instance (Hashable k, Hashable v) => Semigroup (MultiMap k v) where
  (<>) = union

instance (Hashable k, Hashable v) => Monoid (MultiMap k v) where
  mempty = empty

numberOfKeys :: MultiMap k v -> Int
numberOfKeys (MultiMap m) = HashMap.size m

numberOfKeyValuePairs :: MultiMap k v -> Int
numberOfKeyValuePairs (MultiMap m) = F.foldl' (\acc el -> acc + HashSet.size el) 0 m

fromList :: (Hashable k, Hashable v) => [(k, v)] -> MultiMap k v
fromList = L.foldl' (\m (k, v) -> insert k v m) empty

toList :: MultiMap k v -> [(k, v)]
toList (MultiMap hm) = concatMap (\(k, v) -> map (k,) (F.toList v)) $ HashMap.toList hm

toSetList :: MultiMap k v -> [(k, HashSet v)]
toSetList (MultiMap hm) = HashMap.toList hm

fromSetList :: Hashable k => [(k, HashSet v)] -> MultiMap k v
fromSetList setList = MultiMap (HashMap.fromList setList)

empty :: MultiMap k v
empty = MultiMap HashMap.empty

insert :: (Hashable k, Hashable v) => k -> v -> MultiMap k v -> MultiMap k v
insert k v (MultiMap hm) = MultiMap $! HashMap.insert k (HashSet.insert v oldSet) hm
 where
  oldSet = HashMap.findWithDefault HashSet.empty k hm

lookup :: (Hashable k) => k -> MultiMap k v -> HashSet v
lookup k (MultiMap hm) = HashMap.findWithDefault HashSet.empty k hm

delete :: (Hashable k, Hashable v) => k -> v -> MultiMap k v -> MultiMap k v
delete k v (MultiMap hm) = MultiMap $! HashMap.adjust (HashSet.delete v) k hm

keys :: MultiMap k v -> [k]
keys (MultiMap hm) = HashMap.keys hm

-- | Returns all elements of the multimap in an undefined order.
elems :: MultiMap k v -> [v]
elems (MultiMap hm) = concatMap HashSet.toList $ HashMap.elems hm

union :: (Hashable k, Hashable v) => MultiMap k v -> MultiMap k v -> MultiMap k v
union (MultiMap left) (MultiMap right) =
  MultiMap $
    HashMap.unionWith HashSet.union left right

filterWithKey
  :: forall k v
   . (Hashable k)
  => (k -> v -> Bool)
  -> MultiMap k v
  -> MultiMap k v
filterWithKey f (MultiMap m) =
  MultiMap (HashMap.foldlWithKey go HashMap.empty m)
 where
  go :: HashMap k (HashSet v) -> k -> HashSet v -> HashMap k (HashSet v)
  go old k oldSet =
    let newSet = HashSet.filter (f k) oldSet
     in if HashSet.null newSet
          then old
          else HashMap.insert k newSet old

filter
  :: forall k v
   . (Hashable k)
  => (v -> Bool)
  -> MultiMap k v
  -> MultiMap k v
filter f = filterWithKey (\_ v -> f v)

test_filterWithKey :: IO ()
test_filterWithKey =
  do assertEqual r1 $ filterWithKey f mmap1
 where
  f :: a -> Bool -> Bool
  f _ v = v

  r1 :: MultiMap Int Bool
  r1 =
    (MultiMap . HashMap.fromList)
      [ (1, HashSet.singleton True)
      , (3, HashSet.singleton True)
      ]

  mmap1 :: MultiMap Int Bool
  mmap1 =
    fromList
      [ (1, True)
      , (2, False)
      , (3, True)
      , (3, False)
      ]

mmap1 :: MultiMap Int Int
mmap1 =
  fromList
    [ (1, 1)
    , (1, 2)
    , (1, 3)
    , (2, 3)
    ]

test_numberOfKeysAndValues :: IO ()
test_numberOfKeysAndValues =
  do
    assertEqual 2 $ numberOfKeys mmap1
    assertEqual 4 $ numberOfKeyValuePairs mmap1

test_elems :: IO ()
test_elems =
  do
    assertEqual (MSet.fromList [1, 2, 3, 3]) $ MSet.fromList $ elems mmap1
    assertEqual (MSet.fromList [1, 2, 3]) $ MSet.fromList $ elems mmap2
    assertEqual (MSet.fromList [1, 2, 3]) $ MSet.fromList $ elems mmap3
    assertEqual (MSet.fromList [1, 1, 1]) $ MSet.fromList $ elems mmap4
    assertEqual [] $ elems $ fromList ([] :: [(Int, Int)])
 where
  mmap2 :: MultiMap Int Int
  mmap2 =
    fromList
      [ (1, 1)
      , (2, 2)
      , (2, 3)
      ]

  mmap3 :: MultiMap Int Int
  mmap3 =
    fromList
      [ (1, 1)
      , (1, 2)
      , (1, 3)
      ]

  mmap4 :: MultiMap Int Int
  mmap4 =
    fromList
      [ (1, 1)
      , (2, 1)
      , (3, 1)
      ]
