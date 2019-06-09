module Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Data.Default (def)
import Neovim.Plugin (Plugin)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import qualified Ribosome.Test.Embed as Ribosome (integrationSpec, integrationSpecDef)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiSpec, tmuxSpec)
import Ribosome.Test.Unit (unitSpec)

import Config (defaultTestConfig, defaultTestConfigWith, testConf)
import Uracil.Data.Env (Env, Uracil)
import Uracil.Data.Error (Error)
import Uracil.Init (initialize'')
import Uracil.Plugin (plugin')

specConfig :: TestConfig -> Env -> Uracil () -> IO ()
specConfig =
  unitSpec

spec :: Env -> Uracil () -> IO ()
spec =
  specConfig defaultTestConfig

withTempDir :: Env -> (Env -> IO ()) -> IO ()
withTempDir env thunk =
  thunk env

specWith :: Env -> Uracil () -> Vars -> IO ()
specWith env thunk vars =
  withTempDir env run
  where
    run env' =
      unitSpec (defaultTestConfigWith vars) env' thunk

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
tmuxSpec reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxSpec (testConf reconf) env thunk

tmuxGuiSpec :: (TestConfig -> TestConfig) -> Uracil () -> IO ()
tmuxGuiSpec reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxGuiSpec (testConf reconf) env thunk

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
  MonadIO m =>
  ReportError e =>
  RpcHandler e (Ribosome Env) m =>
  MonadDeepError e Error m =>
  m () ->
  IO ()
integrationSpecDef thunk = do
  ribo <- newRibosome "uracil" def
  integrationSpec pure (plugin' ribo) (initialize'' *> thunk)
