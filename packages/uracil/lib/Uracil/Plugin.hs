module Uracil.Plugin where

import Chiasma.Data.Ident (Ident)
import Conc (Lock, interpretAtomic, interpretLockReentrant, withAsync_)
import Exon (exon)
import qualified Log
import Ribosome (
  Event,
  Execution (Async, Sync),
  Reports,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  rpcAutocmd,
  rpcCommand,
  rpcError,
  rpcFunction,
  runNvimPluginIO,
  )
import Ribosome.Menu (ModalWindowMenus, NvimMenus, interpretMenus, interpretWindowMenu)

import Uracil.Data.Env (Env)
import Uracil.Data.PasteLock (PasteLock)
import Uracil.Diag (uraDiag)
import Uracil.Paste (PasteStack, syncClipboard, uraPaste, uraPasteFor, uraPpaste, uraPpasteFor, uraStopPaste)
import Uracil.Yank (uraYank)
import Uracil.YankMenu (uraYankMenu, uraYankMenuFor)

type UracilStack =
  [
    Lock @@ PasteLock,
    AtomicState Env
  ]

type UracilProdStack =
  '[
    ModalWindowMenus Ident !! RpcError
  ] ++ NvimMenus ++ UracilStack

handlers ::
  Members PasteStack r =>
  Members UracilProdStack r =>
  Members [Reports, Mask, Race, Final IO] r =>
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
    -- This must be Sync because the handler reads the variable `v:event`, which is only set while the autocmd is active
    rpcAutocmd "UraYank" Sync "TextYankPost" def uraYank,
    rpcAutocmd "UraStopPaste" Async "CursorMoved" def uraStopPaste
  ]

prepare ::
  Members [Rpc !! RpcError, AtomicState Env, Log, Embed IO] r =>
  Sem r ()
prepare =
  syncClipboard !! \ e -> Log.error [exon|Couldn't sync the clipboard: #{rpcError e}|]

interpretUracilStack ::
  Members [Rpc !! RpcError, Log, Resource, Mask, Race, Async, Embed IO] r =>
  InterpretersFor UracilStack r
interpretUracilStack =
  interpretAtomic def .
  interpretLockReentrant . untag

interpretUracilProdStack ::
  Members [EventConsumer Event, Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError] r =>
  Members [Rpc !! RpcError, Log, Resource, Mask, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor UracilProdStack r
interpretUracilProdStack =
  interpretUracilStack .
  interpretWindowMenu .
  interpretMenus .
  withAsync_ prepare

main :: IO ()
main =
  runNvimPluginIO @UracilProdStack "uracil" interpretUracilProdStack handlers
