module Control.Computations.CompEngine.Utils.VerList (
  VerList,
  vl_dependentsOfVersion,
  mkVerList,
  insIntoVerList,
  delFromVerList,
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
import qualified Data.HashSet as HashSet
import Data.Hashable

{- | A VerList maps a version of a dependency to the computations that used the dependency in this
 specific version.  A VerList should never be the empty map.
-}
newtype VerList a v = VerList
  { vl_dependentsOfVersion :: (HashMap v (HashSet a))
  }
  deriving (Show, Eq)

mkVerList :: HashMap v (HashSet a) -> VerList a v
mkVerList = VerList

insIntoVerList
  :: (Hashable a, Hashable v)
  => a
  -> v
  -> Maybe (VerList a v)
  -> VerList a v
insIntoVerList key newV mvl =
  VerList
    { vl_dependentsOfVersion =
        case mvl of
          Nothing -> HashMap.singleton newV (HashSet.singleton key)
          Just vl -> HashMap.insertWith HashSet.union newV (HashSet.singleton key) (vl_dependentsOfVersion vl)
    }

delFromVerList
  :: (Hashable a, Hashable v)
  => a
  -> v
  -> VerList a v
  -> Maybe (VerList a v)
delFromVerList key oldV vl =
  let newDepOfVers = HashMap.update f oldV (vl_dependentsOfVersion vl)
   in if HashMap.null newDepOfVers
        then Nothing
        else Just (vl{vl_dependentsOfVersion = newDepOfVers})
 where
  f oldKeys =
    let newKeys = HashSet.delete key oldKeys
     in if HashSet.null newKeys then Nothing else Just newKeys
