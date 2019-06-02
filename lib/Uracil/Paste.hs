module Uracil.Paste where

import Chiasma.Data.Ident (Ident)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Data.YankError (YankError)
import Uracil.Yank (loadYank, yankByIdent)

pasteWith ::
  NvimE e m =>
  Text ->
  Yank ->
  m ()
pasteWith cmd yank =
  loadYank "\"" yank *>
  vimCommand ("normal! " <> cmd)

paste ::
  NvimE e m =>
  Yank ->
  m ()
paste =
  pasteWith "p"

ppaste ::
  NvimE e m =>
  Yank ->
  m ()
ppaste =
  pasteWith "P"

pasteIdent ::
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m ()
pasteIdent =
  paste <=< yankByIdent

ppasteIdent ::
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m ()
ppasteIdent =
  ppaste <=< yankByIdent

uraPaste :: m ()
uraPaste =
  undefined
