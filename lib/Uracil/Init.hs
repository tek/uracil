module Uracil.Init where

import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(def))
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Monad.Ribo (RNeovim, runRibo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Orphans ()
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Uracil.Data.Env (Env, Uracil)

initialize'' :: Uracil ()
initialize'' =
  return ()

initialize' :: RNeovim Env (Ribosome Env)
initialize' = do
  result <- runRibo initialize''
  reportError' "init" result
  asks' customConfig

initialize :: Neovim e (Ribosome Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "ura" def
  retypeNeovim (const ribo) initialize'
