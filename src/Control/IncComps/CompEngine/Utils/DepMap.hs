{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.IncComps.CompEngine.Utils.DepMap (
  DepMap (..),
  IsDep (..),
  IsDepConstraints,
  VerList (..),
  empty,
  insert,
  insert',
  dependencies,
  dependents,
  stale,
  keys,
  versions,
  delete,
  dependents',
  depKeys,
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine.Utils.VerList

----------------------------------------
-- EXTERNAL
----------------------------------------

import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

type IsDepConstraints a = (Show a, Eq a, Hashable a)

class
  ( IsDepConstraints a
  , IsDepConstraints (DepKey a)
  , IsDepConstraints (DepVer a)
  ) =>
  IsDep a
  where
  type DepKey a
  type DepVer a
  depKey :: a -> DepKey a
  depVer :: a -> DepVer a

data DepMap a b = DepMap
  { dm_fwdDeps :: HashMap a (HashSet b)
  , dm_revDeps :: HashMap (DepKey b) (VerList a (DepVer b))
  }
  deriving (Typeable)

deriving instance (Show a, IsDep b) => Show (DepMap a b)

empty :: DepMap a b
empty = DepMap HashMap.empty HashMap.empty

insert :: (Hashable a, IsDep b) => a -> HashSet b -> DepMap a b -> DepMap a b
insert a curDeps dm =
  let (m, _, _) = insert' a curDeps dm
   in m

{- | `insert' x ys m' inserts a new mapping where 'x' depends on 'ys' and returns
 a triple of the new DepMap, the set of dependencies 'x' had previously (if it was present)
 and a set of dependencies that are no longer referenced by any key.
-}
insert'
  :: forall a b
   . (Hashable a, IsDep b)
  => a
  -> HashSet b
  -> DepMap a b
  -> (DepMap a b, Maybe (HashSet b), HashSet (DepKey b))
insert' a curDeps dm =
  let oldDeps' = dependencies a dm
      oldDeps = fromMaybe HashSet.empty oldDeps'
      addDeps = curDeps `HashSet.difference` oldDeps
      delDeps = oldDeps `HashSet.difference` curDeps
      oldRevDeps = dm_revDeps dm
      (tmpRevDeps, garbage) =
        let f (m, garbage) d@(depKey -> k) =
              case HashMap.lookup k m of
                Nothing ->
                  error
                    ( "This should not happen.\n"
                        ++ show oldDeps
                        ++ "\n"
                        ++ show curDeps
                        ++ "\n"
                        ++ show k
                    )
                -- (m, garbage)
                Just curVerList ->
                  case delFromVerList a (depVer d) curVerList of
                    Nothing ->
                      ( HashMap.delete k m
                      , HashSet.insert k garbage
                      )
                    Just newVerList ->
                      (HashMap.insert k newVerList m, garbage)
         in (F.foldl' f (oldRevDeps, HashSet.empty) delDeps)
      newRevDeps =
        let f m d = HashMap.alter (g d) (depKey d) m
            g d = Just . insIntoVerList a (depVer d)
         in F.foldl' f tmpRevDeps addDeps
   in ( DepMap
          { dm_fwdDeps = HashMap.insert a curDeps (dm_fwdDeps dm)
          , dm_revDeps = newRevDeps
          }
      , oldDeps'
      , garbage `HashSet.difference` (HashSet.map depKey addDeps)
      )

delete :: (Hashable a, IsDep b) => a -> DepMap a b -> (DepMap a b, HashSet (DepKey b))
delete key dm =
  let (DepMap fwd rev, _, garbage) = insert' key HashSet.empty dm
   in (DepMap (HashMap.delete key fwd) rev, garbage)

keys :: (Hashable a) => DepMap a b -> HashSet a
keys dm = HashSet.fromList $ HashMap.keys (dm_fwdDeps dm)

depKeys :: IsDep b => DepMap a b -> HashSet (DepKey b)
depKeys dm = HashSet.fromList $ HashMap.keys (dm_revDeps dm)

dependencies :: (Hashable a) => a -> DepMap a b -> Maybe (HashSet b)
dependencies k dm = HashMap.lookup k (dm_fwdDeps dm)

versions :: (IsDep b) => DepKey b -> DepMap a b -> HashSet (DepVer b)
versions key dm =
  case HashMap.lookup key (dm_revDeps dm) of
    Nothing -> HashSet.empty
    Just vl -> HashSet.fromList $ HashMap.keys (vl_dependentsOfVersion vl)

dependents :: (IsDep b) => b -> DepMap a b -> HashSet a
dependents d dm =
  case HashMap.lookup (depKey d) (dm_revDeps dm) of
    Nothing -> HashSet.empty
    Just vl -> fromMaybe HashSet.empty (HashMap.lookup (depVer d) (vl_dependentsOfVersion vl))

dependents' :: (IsDep b) => DepKey b -> DepMap a b -> Maybe (VerList a (DepVer b))
dependents' k dm = HashMap.lookup k (dm_revDeps dm)

stale :: (Hashable a, IsDep b) => b -> DepMap a b -> HashSet a
stale d dm =
  case HashMap.lookup (depKey d) (dm_revDeps dm) of
    Nothing ->
      HashSet.empty
    Just vl ->
      let depsOfVer = vl_dependentsOfVersion vl
          woCurVer = HashMap.delete (depVer d) depsOfVer
          result = HashSet.unions (HashMap.elems woCurVer)
       in result
