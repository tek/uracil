module Uracil.Yank where

import Chiasma.Data.Ident (Ident, generateIdent, sameIdent)
import Data.List.Extra (nubOrdOn)
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Ribosome (Handler, Register, RegisterType, Rpc, RpcError, SettingError, Settings, mapReport)
import Ribosome.Api (setregAs, vimGetVvar)
import qualified Ribosome.Register as Register
import qualified Ribosome.Settings as Settings

import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import Uracil.Data.RegEvent (RegEvent (RegEvent))
import Uracil.Data.Yank (Yank (Yank), YankDup (YankDup))
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
  atomicModify' \ s -> s { Env.yanks = nubOrdOn YankDup (yank : s.yanks) }

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
  isNothing <$> atomicGets (.paste)

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
  mapReport @YankError do
    resume_ @RpcError (storeEventIfValid =<< vimGetVvar "event")

allYanks ::
  Member (AtomicState Env) r =>
  Sem r [Yank]
allYanks =
  atomicGets (.yanks)

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
      if op == "d" && regtype == Register.Character && (Text.length <$> lines') == 1 :| []
      then "x"
      else op

yanksFor ::
  Member (AtomicState Env) r =>
  Maybe YankCommand ->
  Sem r [Yank]
yanksFor cmd = do
  atomicGets (filter (matchOperator cmd) . (.yanks))

yanks ::
  Member (AtomicState Env) r =>
  Sem r [Yank]
yanks =
  yanksFor =<< atomicGets (.command)

yankByIdent ::
  Members [AtomicState Env, Stop YankError] r =>
  Ident ->
  Sem r Yank
yankByIdent ident =
  stopNote (YankError.NoSuchYank ident) =<< atomicGets (head . filter (sameIdent ident) . (.yanks))

yankByIndex ::
  Members [AtomicState Env, Stop YankError] r =>
  Int ->
  Sem r Yank
yankByIndex index =
  stopNote (YankError.InvalidYankIndex index) . fetch =<< yanks
  where
    fetch ys =
      ys ^? ix index

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
