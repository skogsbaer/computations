module Control.Computations.Utils.FileStore.Types (
  DocId (..),
  ObjId (..),
  Version,
  mkVersion,
  unVersion,
  incVersion,
  firstVersion,
  zeroVersion,
) where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.Hashable
import Data.Int
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | The ID of a document. The text must only contain characters that are valid in filenames.
newtype DocId = DocId {unDocId :: T.Text}
  deriving (Eq, Hashable)

instance Show DocId where
  showsPrec _ (DocId t) = showString (T.unpack t)

newtype Version = Version {unVersion :: Int64}
  deriving (Eq, Ord, Hashable, Show)

mkVersion :: Int64 -> Version
mkVersion = Version

incVersion :: Version -> Version
incVersion (Version v) = Version (v + 1)

firstVersion :: Version
firstVersion = Version 1

zeroVersion :: Version
zeroVersion = Version 0

data ObjId = ObjId
  { objId_docId :: DocId
  , objId_version :: Version
  }
  deriving (Eq, Generic)

instance Hashable ObjId

instance Show ObjId where
  showsPrec _ (ObjId d (Version i)) =
    shows d
      . showString "/"
      . shows i
