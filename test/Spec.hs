{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.IncComps.Utils.Logging
import Test.Framework

-- Generate with
-- egrep -R -l '^(test|prop)_' src | sed 's|src/||g; s|/|.|g; s|.hs$||g' | sort -u | gawk '{ printf "import {-@ HTF_TESTS @-} %s\n", $0 }'
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.CompFlowRegistry
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.SifCache
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.Tests.TestCompEngine
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.Tests.TestOutputs
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.Tests.TestRevive
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.Tests.TestRun
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.Tests.TestStateIf
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.Utils.OutputsMap
import {-@ HTF_TESTS @-} Control.IncComps.CompEngine.Utils.PriorityAgingQueue
import {-@ HTF_TESTS @-} Control.IncComps.FlowImpls.FileSink
import {-@ HTF_TESTS @-} Control.IncComps.FlowImpls.FileSrc
import {-@ HTF_TESTS @-} Control.IncComps.FlowImpls.SqliteSrc
import {-@ HTF_TESTS @-} Control.IncComps.FlowImpls.TimeSrc
import {-@ HTF_TESTS @-} Control.IncComps.Utils.DataSize
import {-@ HTF_TESTS @-} Control.IncComps.Utils.Fail
import {-@ HTF_TESTS @-} Control.IncComps.Utils.FileWatch
import {-@ HTF_TESTS @-} Control.IncComps.Utils.IOUtils
import {-@ HTF_TESTS @-} Control.IncComps.Utils.MultiMap
import {-@ HTF_TESTS @-} Control.IncComps.Utils.MultiSet
import {-@ HTF_TESTS @-} Control.IncComps.Utils.TimeSpan

main :: IO ()
main = do
  setupLogging WARN
  htfMain htf_importedTests
