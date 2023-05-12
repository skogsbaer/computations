{-# LANGUAGE TypeFamilies #-}

module Control.IncComps.FlowImpls.IOSink (
  unsafeCompIO,
  ioSink,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.IncComps.CompEngine.CompSink
import Control.IncComps.CompEngine.Types
import Control.IncComps.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Proxy
import Data.Void

unsafeCompIO :: IO a -> CompM a
unsafeCompIO action = compSinkReq i (IOSinkReq action)
 where
  i = typedCompSinkId (Proxy @IOSink) ioSinkId

ioSinkId :: CompSinkInstanceId
ioSinkId = CompSinkInstanceId "IOSink"

ioSink :: IOSink
ioSink = IOSink

data IOSink = IOSink

data IOSinkReq a where
  IOSinkReq :: IO a -> IOSinkReq a

instance CompSink IOSink where
  type CompSinkReq IOSink = IOSinkReq
  type CompSinkOut IOSink = Void
  compSinkInstanceId _ = ioSinkId
  compSinkExecute = executeWriteImpl
  compSinkDeleteOutputs _ _ = pure ()
  compSinkListExistingOutputs _ = Some (pure HashSet.empty)

executeWriteImpl :: IOSink -> IOSinkReq a -> IO (HashSet Void, Fail a)
executeWriteImpl IOSink (IOSinkReq action) = do
  res <-
    try action >>= \case
      Left (e :: IOException) -> pure (Fail (show e))
      Right x -> pure (Ok x)
  pure (HashSet.empty, res)
