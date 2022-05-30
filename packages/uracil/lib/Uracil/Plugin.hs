module Uracil.Plugin where

import Conc (interpretAtomic, interpretSyncAs)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Errors (Errors)
import Ribosome.Host.Handler (rpcAutocmd, rpcCommand, rpcFunction)
import Ribosome.Remote (runNvimPluginIO)

import Uracil.Data.Env (Env)
import Uracil.Data.PasteLock (PasteLock (PasteLock))
import Uracil.Diag (uraDiag)
import Uracil.Paste (PasteStack, uraPaste, uraPasteFor, uraPpaste, uraPpasteFor, uraStopPaste)
import Uracil.Yank (uraYank)
import Uracil.YankMenu (uraYankMenu, uraYankMenuFor)

type UracilStack =
  [
    Sync PasteLock,
    AtomicState Env
  ]

conf :: PluginConfig
conf =
  PluginConfig "uracil" def

handlers ::
  Members [Errors, Mask res, Race, Final IO] r =>
  Members PasteStack r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "UraDiag" Async uraDiag,
    rpcFunction "UraPaste" Async uraPaste,
    rpcFunction "UraPpaste" Async uraPpaste,
    rpcFunction "UraPasteFor" Async uraPasteFor,
    rpcFunction "UraPpasteFor" Async uraPpasteFor,
    rpcCommand "UraYankMenu" Async uraYankMenu,
    rpcCommand "UraYankMenuFor" Async uraYankMenuFor,
    -- TODO this was sync in the old version
    rpcAutocmd "UraYank" "TextYankPost" def uraYank,
    rpcAutocmd "UraStopPaste" "CursorMoved" def uraStopPaste
  ]

interpretUracilStack ::
  Members [Race, Embed IO] r =>
  InterpretersFor UracilStack r
interpretUracilStack =
  interpretAtomic def .
  interpretSyncAs PasteLock

uracil :: IO ()
uracil =
  runNvimPluginIO @UracilStack conf mempty mempty handlers interpretUracilStack
