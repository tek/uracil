module Uracil.Init where

import Ribosome.Host (Rpc)

import Uracil.Data.Env (Env)
import Uracil.Paste (syncClipboard)

prepare ::
  Members [Rpc, AtomicState Env, Log, Embed IO] r =>
  Sem r ()
prepare =
  syncClipboard
