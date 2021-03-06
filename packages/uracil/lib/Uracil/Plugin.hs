module Uracil.Plugin where

import qualified Data.Map.Strict as Map (empty)
import Neovim (Neovim, NeovimPlugin, Plugin(..), wrapPlugin)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Plugin (RpcDef, autocmd, cmd, riboPlugin, rpcHandler, rpcHandlerDef, sync)

import Uracil.ContextPaste (uraContextPaste)
import Uracil.Data.Env (Env, Uracil)
import Uracil.Data.Error (Error)
import Uracil.Diag (uraDiag)
import Uracil.Init (initialize)
import Uracil.Paste (uraPaste, uraPasteFor, uraPpaste, uraPpasteFor, uraStopPaste)
import Uracil.Yank (uraYank)
import Uracil.YankMenu (uraYankMenu, uraYankMenuFor)

handleError :: Error -> Uracil ()
handleError =
  reportError "uracil"

rpcHandlers :: [[RpcDef (Ribo Env Error)]]
rpcHandlers =
  [
    $(rpcHandler (cmd []) 'uraDiag),
    $(rpcHandlerDef 'uraPaste),
    $(rpcHandlerDef 'uraPpaste),
    $(rpcHandlerDef 'uraPasteFor),
    $(rpcHandlerDef 'uraPpasteFor),
    $(rpcHandlerDef 'uraContextPaste),
    $(rpcHandler (cmd []) 'uraYankMenuFor),
    $(rpcHandler (cmd []) 'uraYankMenu),
    $(rpcHandler (autocmd "TextYankPost" . sync) 'uraYank),
    $(rpcHandler (autocmd "CursorMoved") 'uraStopPaste)
    ]

plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  riboPlugin "uracil" env rpcHandlers [] handleError Map.empty

plugin :: Neovim e NeovimPlugin
plugin =
  wrapPlugin . plugin' =<< initialize
