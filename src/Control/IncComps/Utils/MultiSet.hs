{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.IncComps.Utils.MultiSet (
  MultiSet (..),
  null,
  empty,
  singleton,
  insert,
  delete,
  member,
  insertGet,
  deleteGet,
  deleteAll,
  count,
  fromList,
  toList,
  distinctElems,
  union,
  difference,
  toSet,
  fromFoldable,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import Data.Hashable
import Test.Framework
import Prelude hiding (null)

newtype MultiSet k = MultiSet {unMultiSet :: HashMap k Int}
  deriving (Show, Eq, Hashable)

empty :: MultiSet a
empty = MultiSet HashMap.empty

null :: MultiSet a -> Bool
null = HashMap.null . unMultiSet

singleton :: Hashable a => a -> MultiSet a
singleton k = MultiSet (HashMap.singleton k 1)

member :: Hashable a => a -> MultiSet a -> Bool
member k = HashMap.member k . unMultiSet

delete :: Hashable a => a -> MultiSet a -> MultiSet a
delete k = snd . deleteGet k

insert :: Hashable a => a -> MultiSet a -> MultiSet a
insert k = MultiSet . HashMap.insertWith (+) k 1 . unMultiSet

insertGet :: (Hashable a) => a -> MultiSet a -> (Bool, MultiSet a)
insertGet k (MultiSet hm) =
  case HashMap.lookup k hm of
    Just _v -> (False, MultiSet (HashMap.insertWith (+) k 1 hm))
    Nothing -> (True, MultiSet (HashMap.insert k 1 hm))

deleteGet :: (Hashable a) => a -> MultiSet a -> (Maybe Int, MultiSet a)
deleteGet k ms@(MultiSet hm) =
  case HashMap.lookup k hm of
    Just v
      | v > 1 -> (Just (v - 1), MultiSet (HashMap.insert k (v - 1) hm))
      | otherwise -> (Just 0, MultiSet (HashMap.delete k hm))
    Nothing -> (Nothing, ms)

count :: (Hashable a) => a -> MultiSet a -> Maybe Int
count k = HashMap.lookup k . unMultiSet

deleteAll :: (Hashable a) => a -> MultiSet a -> MultiSet a
deleteAll k = MultiSet . HashMap.delete k . unMultiSet

fromList :: (Hashable a) => [a] -> MultiSet a
fromList l =
  foldr insert empty l

fromFoldable :: (Foldable f, Hashable a) => f a -> MultiSet a
fromFoldable l =
  foldr insert empty l

distinctElems :: MultiSet a -> [a]
distinctElems (MultiSet hm) =
  HashMap.keys hm

union :: Hashable a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet m1) (MultiSet m2) =
  MultiSet (HashMap.unionWith (+) m1 m2)

difference :: Hashable a => MultiSet a -> MultiSet a -> MultiSet a
difference (MultiSet m1) (MultiSet m2) =
  MultiSet (HashMap.differenceWith f m1 m2)
 where
  f i1 i2 =
    let d = i1 - i2
     in if d < 1 then Nothing else Just d

toSet :: MultiSet a -> HashSet a
toSet (MultiSet m) = HashMap.keysSet m

toList :: MultiSet a -> [a]
toList = mainLoop . HashMap.toList . unMultiSet
 where
  mainLoop [] = []
  mainLoop ((el, count) : xs) = elemLoop (mainLoop xs) el count
  elemLoop cont el count
    | count == 0 = cont
    | otherwise = el : elemLoop cont el (count - 1)

test_basics :: IO ()
test_basics =
  do
    assertEqual "aa" (toList ((singleton 'a') `union` (singleton 'a')))
    assertEqual "aa" (toList (fromList "aa"))
    assertEqual "aabbcc" (toList (fromList "abaccb"))
    assertEqual "aabbcc" (toList ((fromList "aab") `union` (fromList "bcc")))
    assertEqual "abc" (distinctElems (fromList "aaabcc"))
    assertEqual "b" (toList (deleteAll 'a' (fromList "aaab")))
    assertEqual (Just 3) (count 'b' (fromList "aabbbc"))
    assertEqual Nothing (count 'd' (fromList "aabbbc"))
    assertEqual "abbc" (toList (delete 'b' (fromList "abbbc")))
    assertEqual "abbb" (toList (delete 'c' (fromList "abbbc")))
