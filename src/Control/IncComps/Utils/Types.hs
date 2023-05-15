{-# LANGUAGE DeriveAnyClass #-}

module Control.IncComps.Utils.Types (
  -- better name: Misc
  module F,
  Choice (..),
  Option (..),
  option,
  maybeToOption,
  optionToMaybe,
  isNone,
  isSome,
  fromOption,
  showText,
  shorten,
  TypeId (..),
  identifyType,
  identifyProxy,
  Empty (..),
  mkEmpty,
  showHelper1,
  showHelper2,
  readM,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.Utils.Fail as F

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Data
import Data.Hashable
import qualified Data.LargeHashable as LH
import qualified Data.Text as T
import GHC.Generics (Generic)

-- Strict versions of Maybe and Either (note that StrictData is on by default)
data Option a
  = None
  | Some a
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Foldable
    , Traversable
    , Typeable
    , Data
    , Functor
    , Generic
    , Hashable
    , LH.LargeHashable
    )

instance Applicative Option where
  pure = Some
  Some f <*> Some a = Some (f a)
  _ <*> _ = None
  Some _ *> x = x
  _ *> _ = None
  l@(Some _) <* Some _ = l
  _ <* _ = None

instance Alternative Option where
  empty = None
  l@(Some _) <|> _ = l
  _ <|> r = r

instance Semigroup a => Semigroup (Option a) where
  None <> mb = mb
  ma <> None = ma
  Some a <> Some b = Some (a <> b)

instance Monad Option where
  m >>= f =
    case m of
      Some x -> f x
      None -> None

instance MonadPlus Option where
  mzero = None
  mplus x@(Some _) _ = x
  mplus _ y = y

instance MonadFail Option where
  fail _ = None

instance ToJSON a => ToJSON (Option a) where
  toJSON x = toJSON (optionToMaybe x)

instance FromJSON a => FromJSON (Option a) where
  parseJSON val =
    do
      l <- parseJSON val
      pure (maybeToOption l)

optionToMaybe :: Option a -> Maybe a
optionToMaybe (Some a) = Just a
optionToMaybe None = Nothing

maybeToOption :: Maybe a -> Option a
maybeToOption (Just a) = Some a
maybeToOption Nothing = None

option :: b -> (a -> b) -> Option a -> b
option def f opt =
  case opt of
    Some a -> f $! a
    None -> def

fromOption :: a -> Option a -> a
fromOption _ (Some x) = x
fromOption x None = x

isNone :: Option a -> Bool
isNone None = True
isNone (Some _) = False

isSome :: Option a -> Bool
isSome None = False
isSome (Some _) = True

data Choice a b
  = This a
  | That b
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Foldable
    , Traversable
    , Typeable
    , Data
    , Functor
    , Generic
    , Hashable
    , LH.LargeHashable
    )

-- FIXME: Empty needed with Proxy?
data Empty a = Empty deriving (Read, Show, Eq, Ord, Typeable)

mkEmpty :: a -> Empty a
mkEmpty _ = Empty

showText :: Show a => a -> T.Text
showText = T.pack . show

shorten :: Int -> T.Text -> T.Text
shorten maxLen s =
  let actualLen = T.length s
      skipMsg = T.concat ["... (", showText (actualLen - maxLen), " more chars)"]
      skipMsgLen = T.length skipMsg
   in if actualLen <= maxLen + skipMsgLen
        then s
        else T.concat [T.take maxLen s, skipMsg]

newtype TypeId = TypeId {unTypeId :: T.Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, LH.LargeHashable)

identifyType :: forall a. Typeable a => a -> TypeId
identifyType _ = identifyProxy (Proxy :: Proxy a)

identifyProxy :: forall a. Typeable a => Proxy a -> TypeId
identifyProxy p = TypeId (showText (typeRep p))

showHelper2 :: (Show a, Show b) => Int -> String -> a -> b -> ShowS
showHelper2 p name a b =
  showParen (p > 10) $
    showString name
      . showString " "
      . showsPrec 11 a
      . showString " "
      . showsPrec 11 b

showHelper1 :: (Show a) => Int -> String -> a -> ShowS
showHelper1 p name a =
  showParen (p > 10) $
    showString name
      . showString " "
      . showsPrec 11 a

readM :: forall a m. (MonadFail m, Read a, Typeable a) => String -> m a
readM s =
  case [x | (x, "") <- reads s] of
    (x : _) -> pure x
    [] ->
      fail $ "Could not parse " ++ show s ++ " as " ++ ty
 where
  ty = show (typeRep (Proxy :: Proxy a))
