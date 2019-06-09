module Uracil.Yank where

import Chiasma.Data.Ident (Ident, generateIdent, sameIdent)
import qualified Control.Lens as Lens (element, filtered, firstOf, folded)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Ribosome.Api.Register (setregAs)
import Ribosome.Control.Monad.Ribo (prependUnique)
import Ribosome.Data.Register (Register)
import qualified Ribosome.Data.Register as Register (Register(Special, Empty))
import Ribosome.Data.RegisterType (RegisterType)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimGetVvar)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (paste, yanks)
import Uracil.Data.RegEvent (RegEvent(RegEvent))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory, NoSuchYank, EmptyEvent, InvalidYankIndex))

validRegister :: Register -> Bool
validRegister (Register.Special _) =
  True
validRegister Register.Empty =
  True
validRegister _ =
  False

storeYank ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  RegisterType ->
  Register ->
  [Text] ->
  m ()
storeYank regtype register content = do
  text <- hoistMaybe YankError.EmptyEvent (nonEmpty content)
  ident <- generateIdent
  let yank = Yank ident register regtype text
  showDebug "yank" yank
  prependUnique @Env Env.yanks yank

storeEvent ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  RegEvent ->
  m ()
storeEvent (RegEvent _ _ content register regtype) | validRegister register =
  storeYank regtype register content
storeEvent _ =
  return ()

eventValid ::
  NvimE e m =>
  MonadDeepState s Env m =>
  m Bool
eventValid =
  isNothing <$> getL @Env Env.paste

storeEventIfValid ::
  NvimE e m =>
  MonadRibo m =>
  RegEvent ->
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  m ()
storeEventIfValid =
  whenM eventValid . storeEvent

uraYank ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m ()
uraYank =
  ignoreError @DecodeError (storeEventIfValid =<< vimGetVvar "event")

yanks ::
  MonadDeepState s Env m =>
  m [Yank]
yanks =
  getL @Env Env.yanks

nonEmptyYanks ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  m (NonEmpty Yank)
nonEmptyYanks =
  hoistMaybe YankError.EmptyHistory . nonEmpty =<< yanks

yankByIdent ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m Yank
yankByIdent ident =
  hoistMaybe (YankError.NoSuchYank ident) =<< gets @Env (Lens.firstOf lens)
  where
    lens =
      Env.yanks . Lens.folded . Lens.filtered (sameIdent ident)

yankByIndex ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Int ->
  m Yank
yankByIndex index =
  hoistMaybe (YankError.InvalidYankIndex index) =<< gets @Env (Lens.firstOf lens)
  where
    lens =
      Env.yanks . Lens.element index

loadYank ::
  MonadRibo m =>
  NvimE e m =>
  Register ->
  Yank ->
  m ()
loadYank register yank@(Yank _ _ tpe text) = do
  showDebug "loading yank:" yank
  setregAs tpe register text

loadYankIdent ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m ()
loadYankIdent =
  loadYank (Register.Special "\"") <=< yankByIdent
