module Uracil.Init where

import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Monad.Ribo (RNeovim, runRibo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Orphans ()
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Uracil.Data.Env (Env)
import Uracil.Data.Error (Error)

initialize'' ::
  MonadDeepError e Error m =>
  m ()
initialize'' =
  return ()

initialize' :: RNeovim Env (Ribosome Env)
initialize' = do
  result <- runRibo (initialize'' @Error)
  reportError' "init" result
  asks' customConfig

initialize :: Neovim e (Ribosome Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "uracil" def
  retypeNeovim (const ribo) initialize'
