module Uracil.Test.YankTest where

import Polysemy.Test (UnitTest, assertEq)
import Ribosome.Api (nvimInput, setCurrentBufferContent)
import Ribosome.Test (assertWait, testHandlersConf)

import qualified Uracil.Data.Env as Env
import Uracil.Plugin (UracilStack, handlers, interpretUracilStack)
import Uracil.Test.Run (testConfig)

test_yank :: UnitTest
test_yank =
  testHandlersConf @UracilStack (testConfig def) interpretUracilStack handlers do
    setCurrentBufferContent ["1", "2", "3"]
    assertEq 0 . length =<< atomicGets Env.yanks
    _ <- nvimInput "yy"
    assertWait (atomicGets Env.yanks) (assertEq 1 . length)
