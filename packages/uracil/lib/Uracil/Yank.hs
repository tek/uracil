module Uracil.Yank where

import Chiasma.Data.Ident (Ident, generateIdent, sameIdent)
import qualified Control.Lens as Lens
import Data.List (nub)
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Ribosome.Api.Register (setregAs)
import Ribosome.Data.Register (Register)
import qualified Ribosome.Data.Register as Register (Register (Empty, Special))
import qualified Ribosome.Data.RegisterType as RegisterType
import Ribosome.Data.RegisterType (RegisterType)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host (Rpc, RpcError)
import Ribosome.Host.Api.Effect (vimGetVvar)
import Ribosome.Host.Data.HandlerError (mapHandlerError)
import Ribosome.Host.Data.RpcHandler (Handler)

import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import Uracil.Data.RegEvent (RegEvent (RegEvent))
import Uracil.Data.Yank (Yank (Yank))
import Uracil.Data.YankCommand (YankCommand (YankCommand))
import qualified Uracil.Data.YankError as YankError
import Uracil.Data.YankError (YankError)
import qualified Uracil.Settings as Settings

validRegister :: Register -> Bool
validRegister (Register.Special _) =
  True
validRegister Register.Empty =
  True
validRegister _ =
  False

storeYank ::
  Members [AtomicState Env, Log, Embed IO] r =>
  RegisterType ->
  Register ->
  YankCommand ->
  NonEmpty Text ->
  Sem r ()
storeYank regtype register operator content = do
  ident <- generateIdent
  let yank = Yank ident register regtype operator content
  Log.debug [exon|store: #{show yank}|]
  atomicModify' \ s -> s { Env.yanks = nub (yank : Env.yanks s) }

skipEvent ::
  Members [Settings !! SettingError, AtomicState Env, Log, Embed IO] r =>
  [Text] ->
  Sem r ()
skipEvent content = do
  atomicModify' \ s -> s { Env.skip = nonEmpty content }
  resume_ (Settings.update Settings.skipYank False)

storeEvent ::
  Members [Settings !! SettingError, AtomicState Env, Stop YankError, Log, Embed IO] r =>
  RegEvent ->
  Sem r ()
storeEvent (RegEvent _ operator content register regtype) | validRegister register =
  ifM (Settings.or False Settings.skipYank) (skipEvent content) $
  storeYank regtype register operator =<< stopNote YankError.EmptyEvent (nonEmpty content)
storeEvent _ =
  pure ()

eventValid ::
  Member (AtomicState Env) r =>
  Sem r Bool
eventValid =
  isNothing <$> atomicGets Env.paste

storeEventIfValid ::
  Members [Settings !! SettingError, AtomicState Env, Stop YankError, Log, Embed IO] r =>
  RegEvent ->
  Sem r ()
storeEventIfValid =
  whenM eventValid . storeEvent

uraYank ::
  Members [Settings !! SettingError, Rpc !! RpcError, AtomicState Env, Log, Embed IO] r =>
  Handler r ()
uraYank =
  mapHandlerError @YankError do
    resume_ @RpcError (storeEventIfValid =<< vimGetVvar "event")

allYanks ::
  Member (AtomicState Env) r =>
  Sem r [Yank]
allYanks =
  atomicGets Env.yanks

matchOperator ::
  Maybe YankCommand ->
  Yank ->
  Bool
matchOperator Nothing _ =
  True
matchOperator (Just (YankCommand ops)) (Yank _ _ regtype (YankCommand op) lines') =
  effective `Text.isInfixOf` ops
  where
    effective =
      if op == "d" && regtype == RegisterType.Character && (Text.length <$> lines') == 1 :| []
      then "x"
      else op

yanksFor ::
  Member (AtomicState Env) r =>
  Maybe YankCommand ->
  Sem r [Yank]
yanksFor cmd = do
  atomicGets (filter (matchOperator cmd) . Env.yanks)

yanks ::
  Member (AtomicState Env) r =>
  Sem r [Yank]
yanks =
  yanksFor =<< atomicGets Env.command

yankByIdent ::
  Members [AtomicState Env, Stop YankError] r =>
  Ident ->
  Sem r Yank
yankByIdent ident =
  stopNote (YankError.NoSuchYank ident) =<< atomicGets (head . filter (sameIdent ident) . Env.yanks)

yankByIndex ::
  Members [AtomicState Env, Stop YankError] r =>
  Int ->
  Sem r Yank
yankByIndex index =
  stopNote (YankError.InvalidYankIndex index) . fetch =<< yanks
  where
    fetch =
      Lens.firstOf (Lens.element index)

loadYank ::
  Members [Rpc, Log] r =>
  Register ->
  Yank ->
  Sem r ()
loadYank register yank@(Yank _ _ tpe _ content) = do
  Log.debug [exon|load: #{show yank}|]
  setregAs tpe register content

loadYankIdent ::
  Members [Rpc, Log, AtomicState Env, Stop YankError] r =>
  Ident ->
  Sem r ()
loadYankIdent =
  loadYank (Register.Special "\"") <=< yankByIdent

setCommand ::
  Member (AtomicState Env) r =>
  Maybe YankCommand ->
  Sem r ()
setCommand c =
  atomicModify \ ys -> ys { Env.command = c }
