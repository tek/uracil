module Uracil.Yank where

import Chiasma.Data.Ident (Ident, generateIdent, sameIdent)
import qualified Control.Lens as Lens
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Text as Text
import Ribosome.Api.Register (setregAs)
import Ribosome.Config.Setting (settingOr, updateSetting)
import Ribosome.Control.Monad.Ribo (prependUniqueBy)
import Ribosome.Data.Register (Register)
import qualified Ribosome.Data.Register as Register (Register(Special, Empty))
import Ribosome.Data.RegisterType (RegisterType)
import qualified Ribosome.Data.RegisterType as RegisterType
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetVvar)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env
import Uracil.Data.RegEvent (RegEvent(RegEvent))
import Uracil.Data.Yank (Yank(Yank))
import qualified Uracil.Data.Yank as Yank (text)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError
import Uracil.Data.YankOperator (YankOperator(YankOperator))
import qualified Uracil.Settings as Settings

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
  YankOperator ->
  NonEmpty Text ->
  m ()
storeYank regtype register operator content = do
  ident <- generateIdent
  let yank = Yank ident register regtype operator content
  showDebug "yank" yank
  prependUniqueBy @Env Yank.text Env.yanks yank

skipEvent ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  [Text] ->
  m ()
skipEvent content = do
  setL @Env Env.skip (nonEmpty content)
  updateSetting Settings.skipYank False

storeEvent ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  RegEvent ->
  m ()
storeEvent (RegEvent _ operator content register regtype) | validRegister register =
  ifM (settingOr False Settings.skipYank) (skipEvent content) $
  storeYank regtype register operator =<< hoistMaybe YankError.EmptyEvent (nonEmpty content)
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

allYanks ::
  MonadDeepState s Env m =>
  m [Yank]
allYanks =
  getL @Env Env.yanks

yanksFor ::
  MonadDeepState s Env m =>
  Maybe YankOperator ->
  m [Yank]
yanksFor operators = do
  gets @Env (Lens.toListOf lens)
  where
    lens =
      Env.yanks . Lens.folded . Lens.filtered (matchOperator operators)

yanks ::
  MonadDeepState s Env m =>
  m [Yank]
yanks =
  yanksFor =<< getL @Env Env.operators

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

matchOperator :: Maybe YankOperator -> Yank -> Bool
matchOperator Nothing _ =
  True
matchOperator (Just (YankOperator ops)) (Yank _ _ regtype (YankOperator op) lines') =
  effective `Text.isInfixOf` ops
  where
    effective =
      if op == "d" && regtype == RegisterType.Character && (Text.length <$> lines') == 1 :| []
      then "x"
      else op

yankByIndex ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Int ->
  m Yank
yankByIndex index =
  hoistMaybe (YankError.InvalidYankIndex index) . fetch =<< yanks
  where
    fetch =
      Lens.firstOf (Lens.element index)

loadYank ::
  MonadRibo m =>
  NvimE e m =>
  Register ->
  Yank ->
  m ()
loadYank register yank@(Yank _ _ tpe _ text) = do
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

setOperators ::
  MonadDeepState s Env m =>
  Maybe YankOperator ->
  m ()
setOperators =
  setL @Env Env.operators
