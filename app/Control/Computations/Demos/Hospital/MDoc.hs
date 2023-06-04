module Control.Computations.Demos.Hospital.MDoc (
  MDoc (..),
  MSection (..),
  MContent (..),
  MList (..),
  MListItem (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.Types
import Control.Computations.Utils.FileStore.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Computations.Utils.StrictList
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)

data MDoc = MDoc
  { m_title :: T.Text
  , m_sections :: SL MSection
  }
  deriving (Generic)

instance ToJSON MDoc

data MSection = MSection
  { ms_title :: Option T.Text
  , ms_content :: SL MContent
  }
  deriving (Generic)

instance ToJSON MSection

data MContent
  = MContentList MList
  | MContentText T.Text

instance ToJSON MContent where
  toJSON x =
    case x of
      MContentList l -> toJSON l
      MContentText t -> toJSON t

data MList = MList
  {ml_items :: SL MListItem}
  deriving (Generic)

instance ToJSON MList where
  toJSON (MList l) = toJSON l

data MListItem = MListItem
  { ml_docId :: Option DocId
  , ml_text :: T.Text
  }
  deriving (Generic)

instance ToJSON MListItem
