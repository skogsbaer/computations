{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Computations.Utils.Logging
import Test.Framework

-- Generate with
-- egrep -R -l '^(test|prop)_' src | sed 's|src/||g; s|/|.|g; s|.hs$||g' | sort -u | gawk '{ printf "import {-@ HTF_TESTS @-} %s\n", $0 }'
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.CompFlowRegistry
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.SifCache
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestCompEngine
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestOutputs
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestRevive
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestRun
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestStateIf
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Utils.OutputsMap
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Utils.PriorityAgingQueue
import {-@ HTF_TESTS @-} Control.Computations.FlowImpls.FileSink
import {-@ HTF_TESTS @-} Control.Computations.FlowImpls.FileSrc
import {-@ HTF_TESTS @-} Control.Computations.FlowImpls.SqliteSrc
import {-@ HTF_TESTS @-} Control.Computations.FlowImpls.TimeSrc
import {-@ HTF_TESTS @-} Control.Computations.Utils.DataSize
import {-@ HTF_TESTS @-} Control.Computations.Utils.Fail
import {-@ HTF_TESTS @-} Control.Computations.Utils.FileWatch
import {-@ HTF_TESTS @-} Control.Computations.Utils.IOUtils
import {-@ HTF_TESTS @-} Control.Computations.Utils.MultiMap
import {-@ HTF_TESTS @-} Control.Computations.Utils.MultiSet
import {-@ HTF_TESTS @-} Control.Computations.Utils.TimeSpan

main :: IO ()
main = do
  setupLogging WARN
  htfMain htf_importedTests
