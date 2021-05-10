module Uracil.Init where

import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Monad.Ribo (runRibo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Orphans ()
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Uracil.Data.Env (Env)
import Uracil.Data.Error (Error)
import Uracil.Paste (syncClipboard)

prepare ::
  ∀ s e m .
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m ()
prepare =
  syncClipboard

initialize :: Neovim e (Ribosome Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "uracil" def
  retypeNeovim (const ribo) do
    result <- runRibo (prepare @Env @Error)
    reportError' "init" result
    asks' customConfig
