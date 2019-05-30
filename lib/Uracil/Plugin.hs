module Uracil.Plugin where

import qualified Data.Map as Map (empty)
import Neovim (Neovim, NeovimPlugin, Plugin(..), wrapPlugin)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Plugin (RpcDef, cmd, riboPlugin, rpcHandler, rpcHandlerDef)

import Uracil.Data.Env (Env, Uracil)
import Uracil.Data.Error (Error)
import Uracil.Diag (uraDiag)
import Uracil.Init (initialize)
import Uracil.Yank (uraYank)

handleError :: Error -> Uracil ()
handleError =
  reportError "uracil"

rpcHandlers :: [[RpcDef (Ribo Env Error)]]
rpcHandlers =
  [
    $(rpcHandler (cmd []) 'uraDiag),
    $(rpcHandlerDef 'uraYank)
    ]

plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  riboPlugin "ura" env rpcHandlers [] handleError Map.empty

plugin :: Neovim e NeovimPlugin
plugin =
  wrapPlugin . plugin' =<< initialize
