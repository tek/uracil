module Uracil.Plugin where

import Conc (interpretAtomic, interpretSync, interpretSyncAs, withAsync_, Restoration)
import Exon (exon)
import qualified Log
import Ribosome (
  Errors,
  Execution (Async),
  Rpc,
  RpcError (RpcError),
  RpcHandler,
  rpcAutocmd,
  rpcCommand,
  rpcFunction,
  runNvimHandlersIO,
  )
import Ribosome.Menu (PromptListening)

import Uracil.Data.Env (Env)
import Uracil.Data.PasteLock (PasteLock (PasteLock))
import Uracil.Diag (uraDiag)
import Uracil.Paste (PasteStack, syncClipboard, uraPaste, uraPasteFor, uraPpaste, uraPpasteFor, uraStopPaste)
import Uracil.Yank (uraYank)
import Uracil.YankMenu (uraYankMenu, uraYankMenuFor)

type UracilStack =
  [
    Sync PromptListening,
    Sync PasteLock,
    AtomicState Env
  ]

handlers ::
  Members PasteStack r =>
  Members UracilStack r =>
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
    -- TODO this was sync in the old version
    rpcAutocmd "UraYank" Async "TextYankPost" def uraYank,
    rpcAutocmd "UraStopPaste" Async "CursorMoved" def uraStopPaste
  ]

prepare ::
  Members [Rpc !! RpcError, AtomicState Env, Log, Embed IO] r =>
  Sem r ()
prepare =
  syncClipboard !! \ (RpcError e) -> Log.error [exon|Couldn't sync the clipboard: #{e}|]

interpretUracilStack ::
  Members [Rpc !! RpcError, Race, Log, Resource, Async, Embed IO] r =>
  InterpretersFor UracilStack r
interpretUracilStack sem =
  interpretAtomic def $ interpretSyncAs PasteLock $ interpretSync do
    withAsync_ prepare sem

uracil :: IO ()
uracil =
  runNvimHandlersIO @UracilStack "uracil" interpretUracilStack handlers
