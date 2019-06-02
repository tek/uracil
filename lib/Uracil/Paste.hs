module Uracil.Paste where

import Chiasma.Data.Ident (Ident)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Data.YankError (YankError)
import Uracil.Yank (yankByIdent)

paste ::
  NvimE e m =>
  Yank ->
  m ()
paste (Yank _ _ tpe text) = do
  () <- vimCallFunction "setreg" [toMsgpack ("\"" :: Text), toMsgpack text, toMsgpack tpe]
  vimCommand "normal! p"

pasteIdent ::
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m ()
pasteIdent =
  paste <=< yankByIdent

uraPaste :: m ()
uraPaste =
  undefined
