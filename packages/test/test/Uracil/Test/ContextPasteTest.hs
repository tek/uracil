module Uracil.Test.ContextPasteTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Host.Test.Run (runTest)

test_contextPaste :: UnitTest
test_contextPaste =
  runTest do
    True === True
