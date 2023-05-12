{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.IncComps.CompEngine.CompFlow (
  IsCompFlowData,
  ForAnyCompFlow (..),
  applyForEqualIds,
  applyIfEqualIds,
) where

----------------------------------------
-- EXTERNAL
---------------------------------------

import Data.Hashable
import Data.Proxy
import Data.Typeable

type IsCompFlowData a = (Show a, Eq a, Typeable a, Hashable a)

{- | Combines the identity `i` of some `CompSink` or `CompSrc` with values of type
 `Proxy s` and `k s`. Here, `s` is the existentially quantified type variable
 denoting the `CompSink` or `CompSrc`.
 We have the following general assumption:
 Assume `x1` and `x2` have both type `ForAnyCompFlow c i k` and assume that `s1` and `s2`
 are the existentially quantified types of `x1` and `x2`. Then equality of the
 identifies of `x1` and `x2` implies that the types `s1` and `s2` are equal as well.
-}
data ForAnyCompFlow c i k
  = forall s.
    (Typeable s, c s, IsCompFlowData (k s)) =>
    ForAnyCompFlow i (Proxy s) (k s) -- identity and value

instance Show i => Show (ForAnyCompFlow c i k) where
  showsPrec prec (ForAnyCompFlow srcId _ x) =
    showParen (prec >= 10) $
      showString "AnyCompSrcDep { id="
        . shows srcId
        . showString ", dep="
        . shows x
        . showString " }"

instance (Eq i, Typeable k) => Eq (ForAnyCompFlow c i k) where
  c1 == c2 =
    applyIfEqualIds c1 c2 False (const (==))

instance (Hashable i, Typeable k) => Hashable (ForAnyCompFlow c i k) where
  hashWithSalt s (ForAnyCompFlow id proxy x) =
    s `hashWithSalt` id `hashWithSalt` proxy `hashWithSalt` x

{- | `applyForEqualIds c1 c2 def f` applies function f to the content of c1 and c2. Thereby,
 it assumes that equality of the identifies of c1 and c2 implies that types `k s1` and
 `k s2` are equal. Here, `s1` and `s2` are the existintially quantified types of c1 and c2.
-}
applyIfEqualIds
  :: Eq i
  => ForAnyCompFlow c i k
  -> ForAnyCompFlow c i k
  -> a
  -> (forall s. (Typeable s, c s, IsCompFlowData (k s)) => Proxy s -> k s -> k s -> a)
  -> a
applyIfEqualIds (ForAnyCompFlow id1 p1 x1) (ForAnyCompFlow id2 _ x2) def f =
  if id1 == id2
    then case cast x2 of
      Just x2' -> f p1 x1 x2'
      Nothing -> error "ForAnyCompFlow: equality of IDs does not imply equality of types s1 and s2"
    else def

applyForEqualIds
  :: Eq i
  => ForAnyCompFlow c i k
  -> ForAnyCompFlow c i k
  -> (forall s. (Typeable s, c s, IsCompFlowData (k s)) => Proxy s -> k s -> k s -> a)
  -> a
applyForEqualIds x1 x2 f =
  applyIfEqualIds x1 x2 (error ("IDs of ForAnyCompFlow values are expected to be equal")) f
