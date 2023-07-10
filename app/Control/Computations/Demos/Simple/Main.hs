module Control.Computations.Demos.Simple.Main (
  simpleMain
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.FlowImpls.FileSink
import Control.Computations.FlowImpls.FileSrc
import Control.Computations.FlowImpls.IOSink
import Control.Computations.Utils.Logging
import Control.Computations.Utils.IOUtils

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import Data.Proxy
import System.FilePath
import Prelude hiding (readFile, writeFile)

fileSrc :: TypedCompSrcId FileSrc
fileSrc = typedCompSrcId (Proxy @FileSrc) "fileSrc"

fileSink :: TypedCompSinkId FileSink
fileSink = typedCompSinkId (Proxy @FileSink) "fileSink"

numberOfLinesCompDef :: CompDef FilePath Int
numberOfLinesCompDef =
 defineComp "numberOfLines" fullCaching $ \p -> do
  string <- compSrcReq fileSrc (ReadTextFile p)
  pure (length (lines string))

sumCompDef :: Comp FilePath Int
           -> CompDef FilePath Int
sumCompDef c = defineComp "sum" fullCaching $ \p -> do
  string <- compSrcReq fileSrc (ReadTextFile p)
  list <- mapM (evalCompOrFail c) (lines string)
  pure (sum list)

storeCompDef :: Comp FilePath Int -> CompDef () ()
storeCompDef c = do
 defineComp "store" fullCaching $ \() -> do
  i <- evalCompOrFail c "file_list.txt"
  compSinkReq fileSink (WriteTextFile "output.txt" ("number of lines: " ++ show i))

wireAllComps :: CompWireM (Comp () ())
wireAllComps = do
 numberOfLinesC <- wireComp numberOfLinesCompDef
 sumC <- wireComp (sumCompDef numberOfLinesC)
 wireComp (storeCompDef sumC)

withCompFlows :: FilePath -> CompFlowRegistry -> IO () -> IO ()
withCompFlows tgt reg action =
  withFileSrc (defaultFileSrcConfig "fileSrc") $ regSrc reg $ do
    fileSink <- makeFileSink "fileSink" tgt
    registerCompSink reg fileSink
    registerCompSink reg ioSink
    action

simpleMain :: IO ()
simpleMain = withSysTempDir $ \tgt -> do
  logNote ("Target directory: " ++ tgt)
  compDriver (withCompFlows tgt) wireAllComps ()
