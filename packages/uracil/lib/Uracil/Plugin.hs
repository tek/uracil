module Uracil.Plugin where

import Chiasma.Data.Ident (Ident)
import Conc (Lock, Restoration, interpretAtomic, interpretLockReentrant, withAsync_)
import Exon (exon)
import qualified Log
import Ribosome (
  Errors,
  Execution (Async, Sync),
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  rpcAutocmd,
  rpcCommand,
  rpcErrorMessage,
  rpcFunction,
  runNvimHandlersIO,
  )
import Ribosome.Menu (
  MenuState,
  MenusIOEffects,
  NvimMenusIOEffects,
  NvimRenderer,
  interpretMenuStates,
  interpretNvimMenusFinal,
  )
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)

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
  [
    NvimRenderer Ident !! RpcError,
    Scoped () (MenuState Ident)
  ] ++ MenusIOEffects ++ NvimMenusIOEffects ++ UracilStack

handlers ::
  Members PasteStack r =>
  Members UracilProdStack r =>
  Members [Errors, Mask Restoration, Race, Final IO] r =>
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
  syncClipboard !! \ e -> Log.error [exon|Couldn't sync the clipboard: #{rpcErrorMessage e}|]

interpretUracilStack ::
  Members [Rpc !! RpcError, Log, Resource, Mask Restoration, Race, Async, Embed IO] r =>
  InterpretersFor UracilStack r
interpretUracilStack =
  interpretAtomic def .
  interpretLockReentrant . untag

interpretUracilProdStack ::
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError] r =>
  Members [Rpc !! RpcError, Log, Resource, Mask Restoration, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor UracilProdStack r
interpretUracilProdStack =
  interpretUracilStack .
  interpretNvimMenusFinal .
  interpretMenuStates .
  interpretMenuRendererNvim .
  withAsync_ prepare

main :: IO ()
main =
  runNvimHandlersIO @UracilProdStack "uracil" interpretUracilProdStack handlers
