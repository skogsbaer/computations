{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Tests.TestStateIf (
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompEval
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.SimpleStateIf
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Fail

----------------------------------------
-- External
----------------------------------------

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.State.Strict (StateT, runStateT, state)
import qualified Data.ByteString as BS
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)
import Test.Framework

type TestM = StateT SifState IO

data TestStateSrc = TestStateSrc deriving (Show, Read, Eq, Ord, Generic, Typeable)

data VoidRequest a

instance Hashable TestStateSrc

instance CompSrc TestStateSrc where
  type CompSrcReq TestStateSrc = VoidRequest
  type CompSrcKey TestStateSrc = T.Text
  type CompSrcVer TestStateSrc = Int
  compSrcInstanceId _ = CompSrcInstanceId "TestStateSrc"
  compSrcExecute _ act = case act of {}
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry

prepareTest
  :: (CompEngineStateIf TestM -> TestM ())
  -> IO ()
prepareTest test =
  do
    _ <- runStateT (test stateIf) initialSifState
    return ()
 where
  stateIf =
    mkSimpleCompEngineStateIf $
      SimpleStateIf
        { ssif_withState = state
        }

test_capIsNotConsideredStaleAfterItHasBeenDequeuedAsNextCap :: IO ()
test_capIsNotConsideredStaleAfterItHasBeenDequeuedAsNextCap =
  prepareTest $ \sif ->
    do
      capEvaluationStarted sif bar1
      capEvaluationStarted sif foo1
      _ <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 1)]) jval1
      _ <- capEvaluationFinished sif bar1 (mkCompDeps [(foo1, jval1)]) jval1
      -- foo is being recalculated and returns a different result that invalidates bar
      capEvaluationStarted sif foo1
      _ <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 2)]) jval2
      dequeueNextCap sif >>= liftIO . assertEqual (Just (wrapCompAp bar1))
      capEvaluationStarted sif bar1
      -- now bar is being recalculated and dequeues (not stale) foo to evaluate it
      dequeueGivenCap sif foo1 >>= liftIO . assertEqual False
      capEvaluationStarted sif foo1
      -- foo produces a third result but that should not report currently calculated bar as stale
      (staleCaps, _garbage) <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 3)]) jval3
      liftIO $ assertEqual HashSet.empty staleCaps

test_capIsNotConsideredStaleAfterItHasBeenDequeuedByName :: IO ()
test_capIsNotConsideredStaleAfterItHasBeenDequeuedByName =
  prepareTest $ \sif ->
    do
      capEvaluationStarted sif bar1
      capEvaluationStarted sif foo1
      _ <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 1)]) jval1
      _ <- capEvaluationFinished sif bar1 (mkCompDeps [(foo1, jval1)]) jval1
      -- now bar is being recalculated and dequeues (not stale) foo to evaluate it
      capEvaluationStarted sif bar1
      capEvaluationStarted sif foo1
      -- foo produces a different result that should not report currently calculated bar as stale
      (staleCaps, _garbage) <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 2)]) jval2
      liftIO $ assertEqual HashSet.empty staleCaps

test_capBecomesStaleDuringComputationBecauseDependencyChanges :: IO ()
test_capBecomesStaleDuringComputationBecauseDependencyChanges =
  prepareTest $ \sif ->
    do
      -- initial run
      dequeueAndStartCap sif root1 >>= liftIO . assertEqual False
      dequeueAndStartCap sif bar1 >>= liftIO . assertEqual False
      dequeueAndStartCap sif foo1 >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 1)]) jval1
      _ <- capEvaluationFinished sif bar1 (mkCompDeps [(foo1, jval1)]) jval1
      _ <- capEvaluationFinished sif root1 (mkCompDeps [(foo1, jval1), (bar1, jval1)]) jval1
      -- now root is recalcuated
      dequeueAndStartCap sif root1 >>= liftIO . assertEqual False
      -- ...and first calculates foo1 which returns the same result
      dequeueAndStartCap sif foo1 >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 1)]) jval1
      -- ...now the world changes and when bar1 calculates foo1 it returns a different result
      dequeueAndStartCap sif bar1 >>= liftIO . assertEqual False
      dequeueAndStartCap sif foo1 >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif foo1 (mkExtDeps [("ext", 2)]) jval2
      -- bar1 still returns the same result and doesn't invalidate root by itself
      _ <- capEvaluationFinished sif bar1 (mkCompDeps [(foo1, jval2)]) jval1
      _ <- capEvaluationFinished sif root1 (mkCompDeps [(foo1, jval1), (bar1, jval1)]) jval1
      -- root now indirectly depends on two versions of foo and thus SHOULD be stale after the run
      dequeueNextCap sif >>= liftIO . assertEqual (Just (wrapCompAp root1))

test_gc :: IO ()
test_gc =
  prepareTest $ \sif ->
    do
      -- bar1 and bar2 get calculated and both depend on "ext" in version "1"
      capEvaluation sif bar1 (mkExtDeps [("ext", 1)]) jval1
      capEvaluation sif bar2 (mkDeps [("ext", 1)] [(bar1, Nothing)]) jval1
      enqueueStaleCaps sif (mkExtDeps [("ext", 2)]) >>= liftIO . assertEqual 2 . eqiCapSize
      dequeueAndStartCap sif bar2 >>= liftIO . assertEqual True
      -- bar gets calculated but doesn't depend on bar1 anymore
      _ <- capEvaluationFinished sif bar2 (mkExtDeps [("ext", 2)]) jval1
      -- check that (dead) bar1 is not considered stale anymore
      dequeueGivenCap sif bar1 >>= liftIO . assertEqual False

test_impureComputation :: IO ()
test_impureComputation =
  prepareTest $ \sif ->
    do
      capEvaluation sif c (mkExtDeps [("ext", 1)]) jval1
      capEvaluation sif r (mkCompDeps [(c, jval1)]) jval1
      capEvaluation sif c (mkExtDeps [("ext", 1)]) jval2
      dequeueNextCap sif >>= liftIO . assertEqual Nothing

test_modifcationWhileWorkingOnQueue :: IO ()
test_modifcationWhileWorkingOnQueue =
  prepareTest $ \sif ->
    do
      -- bar1 and bar2 get calculated and both depend on "ext" in version "1"
      capEvaluation sif bar1 (mkExtDeps [("ext", 1)]) jval1
      capEvaluation sif bar2 (mkExtDeps [("ext", 1)]) jval1
      -- "ext" changes from version 1 to version 2.  bar1 and bar2 must be stale/queued now
      enqueueStaleCaps sif (mkExtDeps [("ext", 2)]) >>= liftIO . assertEqual 2 . eqiCapSize
      -- dequeue the first and the second in any order
      -- the first uses ext in version 2 but the second already sees version 3
      Just cap1' <- dequeueAndStartNextCap sif
      let cap1
            | cap1' == AnyCompAp bar1 = bar1
            | cap1' == AnyCompAp bar2 = bar2
            | otherwise = error ("dequeueNextCap returned unexpected cap: " ++ show cap1')
      _ <- capEvaluationFinished sif cap1 (mkExtDeps [("ext", 2)]) jval1
      Just cap2' <- dequeueAndStartNextCap sif
      let cap2
            | cap2' == AnyCompAp bar1 = bar1
            | cap2' == AnyCompAp bar2 = bar2
            | otherwise = error ("dequeueNextCap returned unexpected cap: " ++ show cap2')
      _ <- capEvaluationFinished sif cap2 (mkExtDeps [("ext", 3)]) jval1
      -- now the queue is either empty or contains the first cap that became stale by
      -- inserting the second
      nextCap <- dequeueAndStartNextCap sif
      cap3 <-
        case nextCap of
          Nothing ->
            do
              -- nothing is in the queue, tell StateIf about new dep
              enqueueStaleCaps sif (mkExtDeps [("ext", 3)])
                >>= liftIO . assertEqual 1 . eqiCapSize
              -- the first cap must be reevaluated
              Just cap3 <- dequeueNextCap sif
              return cap3
          Just cap3 -> return cap3
      liftIO (assertEqual cap1' cap3)
      -- then everything is up-to-date
      dequeueNextCap sif >>= liftIO . assertEqual Nothing

test_olderVersionInsertedLater :: IO ()
test_olderVersionInsertedLater =
  prepareTest $ \sif ->
    do
      getStale sif >>= liftIO . assertEqual []
      -- r starts...
      capEvaluationStarted sif r
      -- r calls c, c finishes
      capEvaluation sif c (mkExtDeps [("ext", 1)]) jval1
      -- r calls l1
      dequeueAndStartCap sif l1 >>= liftIO . assertEqual False
      -- l1 calls c, c finishes
      dequeueAndStartCap sif c >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif c (mkExtDeps [("ext", 1)]) jval1
      -- l1 finishes
      _ <- capEvaluationFinished sif l1 (mkCompDeps [(c, jval1)]) jval1
      -- r calls l2
      dequeueAndStartCap sif l2 >>= liftIO . assertEqual False
      -- l2 calls c, c finishes with different result and invalidates l1
      -- this can't invalidate r because r is not in the DepMap because it's still being calculated
      dequeueAndStartCap sif c >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif c (mkExtDeps [("ext", 2)]) jval2
      getStale sif >>= liftIO . assertEqual [capId l1]
      -- l2 finishes
      _ <- capEvaluationFinished sif l2 (mkCompDeps [(c, jval2)]) jval2
      -- r finishes
      _ <- capEvaluationFinished sif r (mkCompDeps [(c, jval1), (l1, jval1), (l2, jval2)]) jval1
      getStale sif >>= liftIO . assertEqual [capId l1, capId r]

      -- now the stale l1 is evaluated
      -- l1 calls c, c finishes still with same new result, this invalidates l1 and r
      dequeueAndStartNextCap sif >>= liftIO . assertEqual (Just (AnyCompAp l1))
      getStale sif >>= liftIO . assertEqual [capId r]
      dequeueAndStartCap sif c >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif c (mkExtDeps [("ext", 2)]) jval2
      -- l1 finishes with new result, this invalidates r
      _ <- capEvaluationFinished sif l1 (mkCompDeps [(c, jval2)]) jval2
      getStale sif >>= liftIO . assertEqual [capId r]

      -- r starts...
      dequeueAndStartNextCap sif >>= liftIO . assertEqual (Just (AnyCompAp r))
      getStale sif >>= liftIO . assertEqual []
      -- r calls c, c finishes
      dequeueAndStartCap sif c >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif c noDeps jval2
      -- r calls l1
      dequeueAndStartCap sif l1 >>= liftIO . assertEqual False
      -- l1 calls c, c finishes with same result as before
      dequeueAndStartCap sif c >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif c noDeps jval2
      -- l1 finishes with the same new result
      _ <- capEvaluationFinished sif l1 (mkCompDeps [(c, jval2)]) jval2
      -- r calls l2
      dequeueAndStartCap sif l2 >>= liftIO . assertEqual False
      -- l2 calls c, c finishes with same new result
      dequeueAndStartCap sif c >>= liftIO . assertEqual False
      _ <- capEvaluationFinished sif c noDeps jval2
      -- l2 finishes
      _ <- capEvaluationFinished sif l2 (mkCompDeps [(c, jval2)]) jval2
      -- r finishes
      _ <- capEvaluationFinished sif r (mkCompDeps [(c, jval2), (l1, jval2), (l2, jval2)]) jval1
      getStale sif >>= liftIO . assertEqual []

noDeps :: DepSet
noDeps = mkExtDeps []

test_basics1 :: IO ()
test_basics1 =
  prepareTest $ \sif ->
    do
      -- nothing stored yet
      lookupCapResult sif foo1 >>= liftIO . assertEqual CapNotFound

test_basics2 :: IO ()
test_basics2 =
  prepareTest $ \sif ->
    do
      -- store success and failure without dependencies
      capEvaluation sif foo1 (mkCompDeps ([] :: [(CompAp Int, Maybe Int)])) Nothing
      capEvaluation sif foo2 (mkCompDeps ([] :: [(CompAp Int, Maybe Int)])) jval1
      lookupCapResult sif foo1
        >>= liftIO . assertEqual (CapFound CapFailure)
      lookupCapResult sif foo2
        >>= liftIO . assertEqual (CapFound (CapSuccess (CapValueCached cval1)))

putTestCaps :: CompEngineStateIf TestM -> TestM ()
putTestCaps sif =
  do
    capEvaluation sif bar1 (mkExtDeps [("foo1", 1)]) Nothing
    capEvaluation sif bar2 (mkExtDeps [("foo1", 1)]) jval1
    capEvaluation sif bar3 (mkExtDeps [("foo1", 1), ("foo2", 1)]) jval1
    return ()

test_basics3 :: IO ()
test_basics3 =
  prepareTest $ \sif ->
    do
      putTestCaps sif
      -- test dequeueNextCap
      dequeueNextCap sif >>= liftIO . assertEqual Nothing
      enqueueStaleCaps sif (mkExtDeps [("foo2", 2)]) >>= liftIO . assertEqual 1 . eqiCapSize
      dequeueNextCap sif >>= liftIO . assertEqual (Just (wrapCompAp bar3))
      dequeueNextCap sif >>= liftIO . assertEqual Nothing

test_basics4 :: IO ()
test_basics4 =
  prepareTest $ \sif ->
    do
      putTestCaps sif
      -- test dequeueGivenCap
      enqueueStaleCaps sif (mkExtDeps [("foo2", 3)]) >>= liftIO . assertEqual 1 . eqiCapSize
      dequeueGivenCap sif bar1 >>= liftIO . assertEqual False
      dequeueGivenCap sif bar2 >>= liftIO . assertEqual False
      dequeueGivenCap sif bar3 >>= liftIO . assertEqual True
      dequeueGivenCap sif bar3 >>= liftIO . assertEqual False
      dequeueNextCap sif >>= liftIO . assertEqual Nothing

test_basicsUpdateOfDependencies :: IO ()
test_basicsUpdateOfDependencies =
  prepareTest $ \sif ->
    do
      putTestCaps sif
      capEvaluation sif bar2 (mkExtDeps [("foo1", 1), ("foo2", 1)]) jval1
      enqueueStaleCaps sif (mkExtDeps [("foo2", 2)]) >>= liftIO . assertEqual 2 . eqiCapSize
      dequeueGivenCap sif bar3 >>= liftIO . assertEqual True
      dequeueGivenCap sif bar3 >>= liftIO . assertEqual False
      dequeueGivenCap sif bar2 >>= liftIO . assertEqual True
      dequeueGivenCap sif bar2 >>= liftIO . assertEqual False
      dequeueNextCap sif >>= liftIO . assertEqual Nothing

test_basicsFooBecomesStaleIfItDependsOnBar1AndBar1Changes :: IO ()
test_basicsFooBecomesStaleIfItDependsOnBar1AndBar1Changes =
  prepareTest $ \sif ->
    do
      putTestCaps sif
      dequeueGivenCap sif foo1 >>= liftIO . assertEqual False
      capEvaluation sif foo1 (mkCompDeps [(bar1, Nothing)]) jval1
      dequeueNextCap sif >>= liftIO . assertEqual Nothing
      capEvaluation sif bar1 (mkCompDeps ([] :: [(CompAp Int, Maybe Int)])) jval2
      dequeueNextCap sif >>= liftIO . assertEqual (Just (wrapCompAp foo1))
      dequeueNextCap sif >>= liftIO . assertEqual Nothing

eqiCapSize :: EnqueueInfo -> Int
eqiCapSize = Map.size . ei_affectedCaps

mkCompDeps
  :: IsCompResult a
  => [(CompAp a, Maybe a)]
  -> DepSet
mkCompDeps xs = HashSet.fromList [mkCompDepForCap cap val | (cap, val) <- xs]

mkExtDeps
  :: [(T.Text, Int)]
  -> DepSet
mkExtDeps xs =
  HashSet.fromList (map f xs)
 where
  f :: (T.Text, Int) -> CompEngDep
  f (k, v) = CompEngDepSrc (wrapCompSrcDep TestStateSrc (Dep k v))

mkDeps
  :: IsCompResult c
  => [(T.Text, Int)]
  -> [(CompAp c, Maybe c)]
  -> DepSet
mkDeps xs ys = mkExtDeps xs `HashSet.union` mkCompDeps ys

val1 :: BS.ByteString
val1 = "val1"
jval1 :: Maybe BS.ByteString
jval1 = Just val1

cval1 :: CompApResult BS.ByteString
cval1 = compApResult foo1 val1

val2 :: BS.ByteString
val2 = "val2"

val3 :: BS.ByteString
val3 = "val3"

jval2 :: Maybe BS.ByteString
jval2 = Just val2

jval3 :: Maybe BS.ByteString
jval3 = Just val3

root1, foo1, foo2, bar1, bar2, bar3 :: CompAp BS.ByteString
root1 = mkCompAp rootComp 1
foo1 = mkCompAp fooComp 1
foo2 = mkCompAp fooComp 2
bar1 = mkCompAp barComp 1
bar2 = mkCompAp barComp 2
bar3 = mkCompAp barComp 3

c, l1, l2, r :: CompAp BS.ByteString
c = mkCompAp cComp 0
l1 = mkCompAp l1Comp 1
l2 = mkCompAp l2Comp 2
r = mkCompAp rComp 3

fooComp, barComp, rootComp :: Comp Int BS.ByteString
-- the following is valid Haskell code. What. (http://tvtropes.org/pmwiki/pmwiki.php/Main/FlatWhat)
(_, (fooComp, barComp, rootComp)) =
  failToError $
    runCompDefM $
      do
        fooComp <- defineComp fooCompDef
        barComp <- defineComp (barCompDef fooComp)
        rootComp <- defineComp (rootCompDef fooComp barComp)
        return (fooComp, barComp, rootComp)

cComp, l1Comp, l2Comp, rComp :: Comp Int BS.ByteString
(_, (cComp, l1Comp, l2Comp, rComp)) =
  failToError $
    runCompDefM $
      do
        cComp <- defineComp cCompDef
        l1Comp <- defineComp (lCompDef "l1" cComp)
        l2Comp <- defineComp (lCompDef "l2" cComp)
        rComp <- defineComp (rCompDef cComp l1Comp l2Comp)
        return (cComp, l1Comp, l2Comp, rComp)

cCompDef :: CompDef Int BS.ByteString
cCompDef =
  mkCompDef "c" fullCaching $ \(_ :: Int) ->
    return ""

lCompDef
  :: (Show r, Typeable r, LH.LargeHashable r)
  => String
  -> Comp Int r
  -> CompDef Int r
lCompDef n cComp =
  mkCompDef n fullCaching $ \(p :: Int) ->
    do
      Just x <- evalComp cComp p
      return x

rCompDef
  :: Comp Int BS.ByteString
  -> Comp Int BS.ByteString
  -> Comp Int BS.ByteString
  -> CompDef Int BS.ByteString
rCompDef cComp l1Comp l2Comp =
  mkCompDef "r" fullCaching $ \(p :: Int) ->
    do
      Just x <- evalComp cComp p
      Just y <- evalComp l1Comp p
      Just z <- evalComp l2Comp p
      return (BS.append x (BS.append y z))

fooCompDef :: CompDef Int BS.ByteString
fooCompDef =
  mkCompDef "foo" fullCaching $ \(_ :: Int) ->
    return ""

barCompDef
  :: (Show r, Typeable r, LH.LargeHashable r)
  => Comp Int r
  -> CompDef Int r
barCompDef foo =
  mkCompDef "bar" fullCaching $ \(p :: Int) ->
    do
      Just x <- evalComp foo p
      return x

rootCompDef
  :: Comp Int BS.ByteString
  -> Comp Int BS.ByteString
  -> CompDef Int BS.ByteString
rootCompDef fooComp barComp =
  mkCompDef "root" fullCaching $ \(p :: Int) ->
    do
      Just x <- evalComp fooComp p
      Just y <- evalComp barComp p
      return (BS.append x y)

getStale :: Functor m => CompEngineStateIf m -> m [CapId]
getStale sif = fmap (fmap anyCapId) (getQueue sif)

dequeueAndStartCap :: (IsCompResult a, Monad m) => CompEngineStateIf m -> CompAp a -> m Bool
dequeueAndStartCap sif cap =
  do
    r <- dequeueGivenCap sif cap
    capEvaluationStarted sif cap
    return r

dequeueAndStartNextCap :: Monad m => CompEngineStateIf m -> m (Maybe (AnyCompAp))
dequeueAndStartNextCap sif =
  do
    mCap <- dequeueNextCap sif
    mapM_ (\(AnyCompAp cap) -> capEvaluationStarted sif cap) mCap
    return mCap

capEvaluation
  :: (IsCompResult a, Monad m)
  => CompEngineStateIf m
  -> CompAp a
  -> DepSet
  -> Maybe a
  -> m ()
capEvaluation sif cap deps val =
  do
    capEvaluationStarted sif cap
    _ <- capEvaluationFinished sif cap deps val
    return ()
