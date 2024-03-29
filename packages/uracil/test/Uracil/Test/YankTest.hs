module Uracil.Test.YankTest where

import Polysemy.Test (UnitTest, assertEq, assertJust)
import Ribosome.Api (nvimInput, setCurrentBufferContent)
import Ribosome.Test (assertWait, testPluginConf)

import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import qualified Uracil.Data.Yank as Yank
import Uracil.Plugin (UracilProdStack, handlers, interpretUracilProdStack)
import Uracil.Test.Run (testConfig)

newest ::
  Member (AtomicState Env) r =>
  Sem r (Maybe (NonEmpty Text))
newest =
  fmap (.content) . head <$> atomicGets (.yanks)

yankCount ::
  Member (AtomicState Env) r =>
  Sem r Int
yankCount =
  length <$> atomicGets (.yanks)

test_yank :: UnitTest
test_yank =
  testPluginConf @UracilProdStack (testConfig def) interpretUracilProdStack handlers do
    setCurrentBufferContent ["1", "2", "3"]
    assertEq 0 . length =<< atomicGets (.yanks)
    void (nvimInput "yy")
    assertWait yankCount (assertEq 1)
    void (nvimInput "jyy")
    assertWait newest (assertJust ["2"])
    void (nvimInput "kyy")
    assertWait newest (assertJust ["1"])
    assertEq 2 =<< yankCount
