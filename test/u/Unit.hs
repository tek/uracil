module Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Neovim.Plugin (Plugin)
import Prelude hiding (defaultTestConfig, defaultTestConfigWith, integrationSpec)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import qualified Ribosome.Test.Embed as Ribosome (integrationSpec)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiSpec, tmuxSpec)
import Ribosome.Test.Unit (unitSpec)

import Config (defaultTestConfig, defaultTestConfigWith, testConf)
import Uracil.Data.Env (Env, Uracil)
import Uracil.Data.Error (Error)
import Uracil.Data.YankError (YankError)
import Uracil.Init (initialize'')
import Uracil.Plugin (plugin')

specConfig :: TestConfig -> Env -> Uracil () -> IO ()
specConfig =
  unitSpec

spec :: Env -> Uracil () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Uracil () -> Vars -> IO ()
specWith env thunk vars =
  unitSpec (defaultTestConfigWith vars) env thunk

specWithDef :: Uracil () -> Vars -> IO ()
specWithDef =
  specWith def

specDef :: Uracil () -> IO ()
specDef thunk =
  specWithDef thunk def

withTmux :: Uracil () -> TmuxNative -> Uracil ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- updateSetting tmuxSocket socket
  thunk
withTmux _ _ = fail "no socket in test tmux"

tmuxSpec :: (TestConfig -> TestConfig) -> Uracil () -> IO ()
tmuxSpec reconf =
  Ribosome.tmuxSpec (testConf reconf) def

tmuxGuiSpec :: (TestConfig -> TestConfig) -> Uracil () -> IO ()
tmuxGuiSpec reconf =
  Ribosome.tmuxGuiSpec (testConf reconf) def

tmuxSpecDef :: Uracil () -> IO ()
tmuxSpecDef =
  tmuxSpec def

tmuxGuiSpecDef :: Uracil () -> IO ()
tmuxGuiSpecDef =
  tmuxGuiSpec def

integrationSpec ::
  NvimE e m =>
  MonadIO m =>
  ReportError e =>
  RpcHandler e env m =>
  (TestConfig -> IO TestConfig) ->
  Plugin env ->
  m () ->
  IO ()
integrationSpec reconf plug thunk = do
  conf <- reconf defaultTestConfig
  Ribosome.integrationSpec conf plug thunk

integrationSpecDef ::
  NvimE e m =>
  MonadRibo m =>
  MonadIO m =>
  ReportError e =>
  RpcHandler e (Ribosome Env) m =>
  MonadDeepError e Error m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m () ->
  IO ()
integrationSpecDef thunk = do
  ribo <- newRibosome "uracil" def
  integrationSpec pure (plugin' ribo) (initialize'' *> thunk)
