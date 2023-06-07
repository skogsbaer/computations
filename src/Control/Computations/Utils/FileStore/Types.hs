module Control.Computations.Utils.FileStore.Types (
  DocId (..),
  mkDocId,
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

import Data.Aeson
import Data.Hashable
import Data.Int
import Data.LargeHashable
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | The ID of a document. The text must only contain characters that are valid in filenames.
newtype DocId = DocId {unDocId :: T.Text}
  deriving newtype (Eq, Hashable, LargeHashable, ToJSON, ToJSONKey)

mkDocId :: T.Text -> DocId
mkDocId = DocId

instance Show DocId where
  showsPrec _ (DocId t) = showString (T.unpack t)

newtype Version = Version {unVersion :: Int64}
  deriving (Eq, Ord, Hashable)
  deriving newtype (ToJSON)

instance Show Version where
  showsPrec p (Version v) =
    showParen (p > 10) $
    showString "Version " .
    shows v

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
