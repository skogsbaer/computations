{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Tests.TestOutputs (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompEval
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.TestHelper
import Control.Computations.CompEngine.Types
import Control.Computations.FlowImpls.HashMapFlow
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Framework

test_whenComputationDoesntGenerateOutputAnymoreItIsDeleted :: IO ()
test_whenComputationDoesntGenerateOutputAnymoreItIsDeleted =
  runCompEngineTest compDefs shouldStartNextRun () doTest
 where
  compDefs = defineComp mainCompDef
   where
    parseInt :: Maybe BS.ByteString -> Int
    parseInt x = fromMaybe 0 (x >>= readM . T.unpack . T.decodeUtf8)
    mainCompDef :: CompDef () ()
    mainCompDef =
      mkCompDef "main" inMemoryShowCaching $ \() ->
        do
          start <- liftM parseInt (get "start")
          end <- liftM parseInt (get "end")
          forM_ [start .. end] $ \i ->
            put (T.encodeUtf8 (showText i)) "dummy"
  doTest hmf =
    do
      _ <- hmfLookup hmf "1" >>= assertJust
      _ <- hmfLookup hmf "2" >>= assertJust
      hmfLookup hmf "3" >>= assertNothing
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _ s
    | run == 1 =
        do
          logInfo "generating outputs 1, 2, 3"
          hmfInsert hmf "start" "1"
          hmfInsert hmf "end" "3"
          return (startNextRun, s)
    | run == 2 =
        do
          logInfo "only generating outputs 1, 2"
          hmfInsert hmf "end" "2"
          return (startNextRun, s)
    | otherwise =
        return (noNextRun, s)

isYes :: Maybe BS.ByteString -> Bool
isYes = (Just "yes" ==)

test_ifTwoCompApsProduceTheSameOutputAndOneOfThemDiesTheOutputIsNotDeleted :: IO ()
test_ifTwoCompApsProduceTheSameOutputAndOneOfThemDiesTheOutputIsNotDeleted =
  runCompEngineTest compDefs shouldStartNextRun () doTest
 where
  compDefs =
    do
      subComp <- defineComp subCompDef
      defineComp (mainCompDef subComp)
   where
    subCompDef :: CompDef Bool ()
    subCompDef =
      mkCompDef "sub" inMemoryShowCaching $ \b ->
        do
          pureInfo "Generating dummy output" $
            put "output" (if b then "yes" else "no")
    mainCompDef :: Comp Bool () -> CompDef () ()
    mainCompDef subComp =
      mkCompDef "main" inMemoryShowCaching $ \() ->
        do
          evalCompOrFail subComp False
          flag <- isYes <$> get "flag-main"
          when flag (evalCompOrFail subComp True)
  doTest _hmf = return () -- see asserts in shouldStartNextRun
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _numStale s
    | run == 1 =
        do
          hmfLookup hmf "output" >>= assertJust >>= assertEqual "no"
          hmfInsert hmf "flag-main" "yes"
          return (startNextRun, s)
    | run == 2 =
        do
          hmfLookup hmf "output" >>= assertJust >>= assertEqual "yes"
          hmfInsert hmf "flag-main" "no"
          return (startNextRun, s)
    | run == 3 =
        do
          hmfLookup hmf "output" >>= assertJust >>= assertEqual "yes"
          return (startNextRun, s)
    | otherwise =
        return (noNextRun, s)

test_whenComputationIsNotCalledAnymoreItsOutputIsDeleted :: IO ()
test_whenComputationIsNotCalledAnymoreItsOutputIsDeleted =
  runCompEngineTest compDefs shouldStartNextRun () doTest
 where
  compDefs =
    do
      subComp <- defineComp subCompDef
      defineComp (mainCompDef subComp)
   where
    subCompDef :: CompDef () ()
    subCompDef =
      mkCompDef "sub" inMemoryShowCaching $ \() ->
        pureInfo "Generating dummy output" $
          put "output" "dummy"
    mainCompDef :: Comp () () -> CompDef () ()
    mainCompDef subComp =
      mkCompDef "main" inMemoryShowCaching $ \() ->
        do
          flag <- liftM isYes (get "flag")
          when flag (evalCompOrFail subComp ())
  doTest hmf =
    hmfLookup hmf "output" >>= assertNothing
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _ s
    | run == 1 =
        do
          logInfo "generating output"
          hmfInsert hmf "flag" "yes"
          return (startNextRun, s)
    | run == 2 = return (startNextRun, s)
    | run == 3 =
        do
          logInfo "asserting there is output and setting flag to false"
          _ <- hmfLookup hmf "output" >>= assertJust
          hmfInsert hmf "flag" "no"
          return (startNextRun, s)
    | otherwise = do
        logInfo ("Finished, run=" ++ show run)
        return (noNextRun, s)

test_whenOneComputationGeneratingOutputDiesAndAnotherReplacesItTheOutputIsNotDeleted :: IO ()
test_whenOneComputationGeneratingOutputDiesAndAnotherReplacesItTheOutputIsNotDeleted =
  runCompEngineTest compDefs shouldStartNextRun () doTest
 where
  compDefs =
    do
      subComp <- defineComp subCompDef
      defineComp (mainCompDef subComp)
   where
    subCompDef :: CompDef Bool ()
    subCompDef =
      mkCompDef "sub" inMemoryShowCaching $ \_ ->
        pureInfo "Generating dummy output" $ put "output" "dummy"
    mainCompDef :: Comp Bool () -> CompDef () ()
    mainCompDef subComp =
      mkCompDef "main" inMemoryShowCaching $ \() ->
        do
          flag <- liftM isYes (get "flag")
          evalCompOrFail subComp flag
  doTest hmf =
    hmfLookup hmf "output" >>= assertJust >>= assertEqual "dummy"
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _ s
    | run == 1 =
        do
          doTest hmf
          hmfInsert hmf "flag" "yes"
          return (startNextRun, s)
    | otherwise =
        do
          doTest hmf
          return (noNextRun, s)

test_oneComputationRecomputedButOtherUnreferencesIt :: IO ()
test_oneComputationRecomputedButOtherUnreferencesIt =
  runCompEngineTest compDefs shouldStartNextRun () (const $ return ())
 where
  compDefs =
    do
      commonComp <- defineComp commonCompDef
      subComp <- defineComp (subCompDef commonComp)
      defineComp (mainCompDef commonComp subComp)
   where
    commonCompDef :: CompDef () Bool
    commonCompDef =
      mkCompDef "common" inMemoryShowCaching $ \_ ->
        do
          flag <- liftM isYes (get "flag")
          pureInfo "Generating common output" $
            put "common_output" (C8.pack $ show flag)
          return flag
    subCompDef :: Comp () Bool -> CompDef Bool ()
    subCompDef commonComp =
      mkCompDef "asub" inMemoryShowCaching $ \_ ->
        do
          flag <- evalCompOrFail commonComp ()
          pureInfo "Generating sub output" $
            put "sub_output" (C8.pack $ show flag)
    mainCompDef
      :: Comp () Bool
      -> Comp Bool ()
      -> CompDef () ()
    mainCompDef commonComp subComp =
      mkCompDef "main" inMemoryShowCaching $ \() ->
        do
          flag <- evalCompOrFail commonComp ()
          unless flag $
            pureInfo "Evaluating subComp" (evalCompOrFail subComp flag)
  allOutputExists hmf =
    do
      hmfLookup hmf "sub_output" >>= assertJust >>= assertEqual "False"
      hmfLookup hmf "common_output" >>= assertJust >>= assertEqual "False"
  subOutputGone hmf =
    do
      hmfLookup hmf "sub_output" >>= assertNothing
      hmfLookup hmf "common_output" >>= assertJust >>= assertEqual "True"
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _ s
    | run == 1 =
        do
          allOutputExists hmf
          hmfInsert hmf "flag" "yes"
          return (startNextRun, s)
    | run == 2 =
        do
          subOutputGone hmf
          return (noNextRun, s)
    | otherwise =
        do return (noNextRun, s)
