{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Tests.TestRevive (htf_thisModulesTests) where

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

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import qualified Data.ByteString as BS
import Test.Framework

test_dontDeleteAfterRevival :: IO ()
test_dontDeleteAfterRevival = runCompEngineTest compDefs shouldStartNextRun () doTest
 where
  compDefs =
    do
      writeComp <- defineComp writeCompDef
      intermediateComp <- defineComp (intermediateCompDef writeComp)
      defineComp (mainCompDef writeComp intermediateComp)
   where
    isYes :: Maybe BS.ByteString -> Bool
    isYes = (Just "yes" ==)
    mainCompDef
      :: Comp () ()
      -> Comp () ()
      -> CompDef () ()
    mainCompDef writeComp intermediateComp =
      mkCompDef "main" inMemoryShowCaching $ \() ->
        do
          doSecond <- liftM isYes (get "second")
          evalCompOrFail intermediateComp ()
          when doSecond (evalCompOrFail writeComp ())
          return ()
    intermediateCompDef :: Comp () () -> CompDef () ()
    intermediateCompDef writeComp =
      mkCompDef "intermediate" inMemoryShowCaching $ \() ->
        do
          doFirst <- liftM isYes (get "first")
          when doFirst (evalCompOrFail writeComp ())
    writeCompDef :: CompDef () ()
    writeCompDef =
      mkCompDef "write" inMemoryShowCaching $ \() ->
        put "foo" "bar"
  doTest hmf =
    do
      mRes <- hmfLookup hmf "foo"
      assertEqual (Just "bar") mRes
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _ s
    | run == 1 =
        do
          logInfo "setting first=yes and second=no"
          hmfInsert hmf "first" "yes"
          hmfInsert hmf "second" "no"
          return (startNextRun, s)
    | run == 2 =
        do
          logInfo "setting first=no and second=yes"
          hmfInsert hmf "first" "no"
          hmfInsert hmf "second" "yes"
          return (startNextRun, s)
    | otherwise =
        return (noNextRun, s)
