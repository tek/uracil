module Uracil.Test.ContextPasteTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Test (runTest)

test_contextPaste :: UnitTest
test_contextPaste =
  runTest do
    True === True
