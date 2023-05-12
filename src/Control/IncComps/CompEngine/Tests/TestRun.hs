{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.IncComps.CompEngine.Tests.TestRun (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.IncComps.CompEngine.CacheBehaviors
import Control.IncComps.CompEngine.CompDef
import Control.IncComps.CompEngine.CompEval
import Control.IncComps.CompEngine.Run (CompRunIterationLimit (..), NextRun, noNextRun, startNextRun)
import Control.IncComps.CompEngine.Tests.TestHelper
import Control.IncComps.CompEngine.Types
import Control.IncComps.FlowImpls.HashMapFlow
import Control.IncComps.Utils.Logging
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Framework

type MainC c = c () ()
type WriterC c = c T.Text ()
type DummyC c = c T.Text ()

test_whenComputationDoesntGenerateOutputAnymoreItIsDeleted :: IO ()
test_whenComputationDoesntGenerateOutputAnymoreItIsDeleted =
  do
    (_, startTest) <- initCompEngineTest compDefs
    startTest (CompRunLimitIterationsTo 2) shouldStartNextRun () doTest
 where
  compDefs =
    do
      writerC <- defineComp writerCompDef
      dummyC <- defineComp dummyCompDef
      defineComp (mainCompDef dummyC writerC)
   where
    parseBool :: Maybe BS.ByteString -> Bool
    parseBool x = Just True == (x >>= readM . T.unpack . T.decodeUtf8)
    mainCompDef :: DummyC Comp -> WriterC Comp -> MainC CompDef
    mainCompDef dummyComp writerComp =
      mkComp "main" inMemoryShowCaching $ \() ->
        do
          Just _ <- evalComp dummyComp "dummy1"
          Just _ <- evalComp dummyComp "dummy2"
          Just _ <- evalComp writerComp "first"
          Just _ <- evalComp writerComp "second"
          return ()
    dummyCompDef :: DummyC CompDef
    dummyCompDef =
      mkComp "dummy" inMemoryShowCaching $ \_ ->
        do
          _ <- liftM parseBool (get (T.encodeUtf8 "writeFlag"))
          return ()
    writerCompDef :: WriterC CompDef
    writerCompDef =
      mkComp "writer" inMemoryShowCaching $ \key ->
        do
          writeFlag <- liftM parseBool (get (T.encodeUtf8 "writeFlag"))
          when writeFlag (put (T.encodeUtf8 key) "value")
          return ()
  doTest _hmf = return ()
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ staleCaps s
    | run == 1 =
        do
          logInfo "Setting writeFlag to True.  This will lead to 2 stale dummy comps."
          hmfInsert hmf "writeFlag" "True"
          return (startNextRun, s)
    | run == 2 =
        do
          assertEqual 2 staleCaps
          return (startNextRun, s)
    | run == 3 =
        do
          assertEqual 0 staleCaps
          -- assert that outputs have been created
          _ <- hmfLookup hmf "first" >>= assertJust
          _ <- hmfLookup hmf "second" >>= assertJust
          logInfo "Setting writeFlag to False.  This will lead to 4 stale comps."
          hmfInsert hmf "writeFlag" "False"
          return (startNextRun, s)
    | run == 4 =
        do
          assertEqual 2 staleCaps
          -- assert that outputs have not yet been deleted (there are stale caps)
          _ <- hmfLookup hmf "first" >>= assertJust
          _ <- hmfLookup hmf "second" >>= assertJust
          return (startNextRun, s)
    | otherwise =
        do
          assertEqual 0 staleCaps
          -- assert that outputs have been deleted (there are no stale caps left)
          hmfLookup hmf "first" >>= assertNothing
          hmfLookup hmf "second" >>= assertNothing
          return (noNextRun, s)
