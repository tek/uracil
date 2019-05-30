module Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Data.Default (def)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiSpec, tmuxSpec)
import Ribosome.Test.Unit (unitSpec)
import UnliftIO (throwString)

import Config (defaultTestConfig, defaultTestConfigWith, testConf)
import Uracil.Data.Env (Env, Uracil)

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
withTmux _ _ = throwString "no socket in test tmux"

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
