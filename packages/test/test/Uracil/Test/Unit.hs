module Uracil.Test.Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Hedgehog (TestT)
import Neovim.Plugin (Plugin)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import qualified Ribosome.Test.Embed as Ribosome (integrationTest)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiTest, tmuxTest)
import Ribosome.Test.Tmux (RiboTesting)
import Ribosome.Test.Unit (unitTest)

import Uracil.Data.Env (Env, Uracil)
import Uracil.Data.Error (Error)
import Uracil.Init (prepare)
import Uracil.Plugin (plugin')
import Uracil.Test.Config (defaultTestConfig, defaultTestConfigWith, testConf)

type UracilTest a = TestT (Ribo Env Error) a
type UracilTestingEnv env m n =
  RiboTesting Error env m n
type UracilTesting m n =
  UracilTestingEnv (Ribosome Env) m n

test ::
  UracilTesting m n =>
  Env ->
  TestT n a ->
  TestT m a
test =
  unitTest defaultTestConfig

testWith ::
  UracilTesting m n =>
  Env ->
  TestT n a ->
  Vars ->
  TestT m a
testWith env thunk vars =
  unitTest (defaultTestConfigWith vars) env thunk

testWithDef ::
  UracilTesting m n =>
  TestT n a ->
  Vars ->
  TestT m a
testWithDef =
  testWith def

testDef ::
  UracilTesting m n =>
  TestT n a ->
  TestT m a
testDef thunk =
  testWithDef thunk def

withTmux :: Uracil () -> TmuxNative -> Uracil ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- updateSetting tmuxSocket socket
  thunk
withTmux _ _ = fail "no socket in test tmux"

tmuxTest ::
  UracilTesting m n =>
  (TestConfig -> TestConfig) ->
  TestT n a ->
  TestT m a
tmuxTest reconf =
  Ribosome.tmuxTest (testConf reconf) def

tmuxGuiTest ::
  UracilTesting m n =>
  (TestConfig -> TestConfig) ->
  TestT n a ->
  TestT m a
tmuxGuiTest reconf =
  Ribosome.tmuxGuiTest (testConf reconf) def

tmuxTestDef ::
  UracilTesting m n =>
  TestT n a ->
  TestT m a
tmuxTestDef =
  tmuxTest def

tmuxGuiTestDef ::
  UracilTesting m n =>
  TestT n a ->
  TestT m a
tmuxGuiTestDef =
  tmuxGuiTest def

integrationTest ::
  UracilTestingEnv env m n =>
  (TestConfig -> IO TestConfig) ->
  Plugin env ->
  TestT n a ->
  TestT m a
integrationTest reconf plug thunk = do
  conf <- liftIO (reconf defaultTestConfig)
  Ribosome.integrationTest conf plug thunk

integrationTestDef ::
  UracilTestingEnv (Ribosome Env) m n =>
  MonadDeepState s Env n =>
  TestT n a ->
  TestT m a
integrationTestDef thunk = do
  ribo <- newRibosome "uracil" def
  integrationTest pure (plugin' ribo) (prepare *> thunk)
