module Uracil.Test.ContextPasteTest where

import Ribosome.Test.Run (UnitTest)
import Uracil.Test.Unit (UracilTest, testDef)
import Hedgehog ((===))

contextPasteTest :: UracilTest ()
contextPasteTest = do
  True === True

test_contextPaste :: UnitTest
test_contextPaste =
  testDef contextPasteTest
