module Uracil.Paste where

import Chiasma.Data.Ident (Ident)
import qualified Chronos
import Conc (Lock, lock, lockOrSkip)
import Control.Monad.Extra (andM)
import qualified Data.Text as Text (isInfixOf)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Polysemy.Time (MilliSeconds, convert)
import Ribosome (
  Handler,
  Report,
  Rpc,
  RpcError,
  Scratch,
  SettingError,
  Settings,
  mapReport,
  noautocmd,
  pluginLogReports,
  resumeReport,
  )
import Ribosome.Api (normal, redraw, undo, unnamedRegister, vimGetOption, visualModeActive)
import Ribosome.Register (Register, registerRepr)
import qualified Ribosome.Register as Register (Register (..))
import qualified Ribosome.Scratch as Scratch
import qualified Ribosome.Settings as Settings
import qualified Time

import Uracil.Clipboard (syncClipboard)
import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import Uracil.Data.Paste (Paste (Paste))
import Uracil.Data.PasteLock (PasteLock)
import Uracil.Data.Yank (Yank)
import Uracil.Data.YankCommand (YankCommand)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError (EmptyHistory))
import qualified Uracil.Settings as Settings (pasteTimeout, pasteTimeoutMillis)
import Uracil.Yank (loadYank, setCommand, yankByIdent, yankByIndex, yanks)
import Uracil.YankScratch (deleteYankScratch, ensureYankScratch, selectYankInScratch)

defaultRegister ::
  Member Rpc r =>
  Sem r Register
defaultRegister =
  Register.Special . decide <$> vimGetOption "clipboard"
  where
    decide "unnamed" =
      "*"
    decide a | Text.isInfixOf "unnamedplus" a =
      "+"
    decide _ =
      "\""

pasteWith ::
  Members [Rpc !! e, Rpc, Log] r =>
  Text ->
  Yank ->
  Sem r ()
pasteWith cmd yank = do
  register <- defaultRegister
  loadYank register yank
  resume_ (noautocmd $ normal (registerRepr register <> cmd))
  loadYank unnamedRegister yank

paste ::
  Members [Rpc !! e, Rpc, Log] r =>
  Yank ->
  Sem r ()
paste =
  pasteWith "p"

ppaste ::
  Members [Rpc !! e, Rpc, Log] r =>
  Yank ->
  Sem r ()
ppaste =
  pasteWith "P"

pasteIdent ::
  Members [AtomicState Env, Rpc !! e, Rpc, Log, Stop YankError] r =>
  Ident ->
  Sem r ()
pasteIdent =
  paste <=< yankByIdent

ppasteIdent ::
  Members [AtomicState Env, Rpc !! e, Rpc, Log, Stop YankError] r =>
  Ident ->
  Sem r ()
ppasteIdent =
  ppaste <=< yankByIdent

currentPaste ::
  Member (AtomicState Env) r =>
  Sem r (Maybe Paste)
currentPaste =
  atomicGets (.paste)

pasteActive ::
  Member (AtomicState Env) r =>
  Sem r Bool
pasteActive =
  isJust <$> currentPaste

pasteHasTimedOut ::
  Member ChronosTime r =>
  MilliSeconds ->
  Chronos.Time ->
  Sem r Bool
pasteHasTimedOut timeout updated = do
  diff <- Time.since updated
  pure (diff >= timeout)

shouldCancelPaste ::
  Members [AtomicState Env, ChronosTime] r =>
  MilliSeconds ->
  Ident ->
  Sem r Bool
shouldCancelPaste timeout ident =
  fmap (fromMaybe False) <$> traverse check =<< currentPaste
  where
    check (Paste pasteIdent' _ updated _ _) =
      andM [pasteHasTimedOut timeout updated, pure $ ident == pasteIdent']

moveYankToHistoryHead ::
  Member (AtomicState Env) r =>
  Int ->
  Sem r ()
moveYankToHistoryHead index =
  atomicModify' (#yanks %~ move)
  where
    move ys =
      take 1 post <> pre <> drop 1 post
      where
        (pre, post) = splitAt index ys

movePastedToHistoryHead ::
  Member (AtomicState Env) r =>
  Sem r ()
movePastedToHistoryHead =
  currentPaste >>= traverse_ \ (Paste _ index _ _ _) ->
    moveYankToHistoryHead index

cancelPaste ::
  Members [Scratch, AtomicState Env, Log] r =>
  Sem r ()
cancelPaste = do
  Log.debug "cancelling paste"
  movePastedToHistoryHead
  atomicModify' (#paste .~ Nothing)
  deleteYankScratch
  setCommand Nothing

cancelPasteAfter ::
  Members [Scratch, AtomicState Env, ChronosTime, Log] r =>
  MilliSeconds ->
  Ident ->
  Sem r ()
cancelPasteAfter timeout ident =
  whenM (shouldCancelPaste timeout ident) cancelPaste

pasteTimeout ::
  Members [Settings !! SettingError, Settings] r =>
  Sem r MilliSeconds
pasteTimeout = do
  durationMillis <- Settings.maybe Settings.pasteTimeoutMillis
  maybe (convert <$> Settings.get Settings.pasteTimeout) pure durationMillis

waitAndCancelPaste ::
  Members [Settings !! SettingError, Scratch, AtomicState Env, ChronosTime, Log] r =>
  Member Settings r =>
  Ident ->
  Sem r ()
waitAndCancelPaste ident = do
  duration <- pasteTimeout
  Time.sleep duration
  cancelPasteAfter duration ident

logPaste ::
  Member Log r =>
  Bool ->
  Int ->
  Bool ->
  Yank ->
  Sem r ()
logPaste update index visual yank =
  Log.debug [exon|#{action} in #{visualT} mode: #{show yank}|]
  where
    action =
      if update then [exon|repasting with index #{show index}|] else "starting paste"
    visualT =
      if visual then "visual" else "normal"

insertPaste ::
  Members [Scratch, AtomicState Env, Stop YankError, Stop Report, Log, ChronosTime, Async, Input Ident] r =>
  Members [Settings !! SettingError, Settings, Rpc] r =>
  Bool ->
  (Yank -> Sem r ()) ->
  Int ->
  Sem r ()
insertPaste isUpdate paster index = do
  visual <- visualModeActive
  yank <- yankByIndex index
  logPaste isUpdate index visual yank
  paster yank
  scratch <- ensureYankScratch
  updated <- Time.now
  ident <- input
  atomicModify' (#paste ?~ Paste ident index updated scratch.id visual)
  selectYankInScratch scratch index
  redraw
  void (async (waitAndCancelPaste ident))

repaste ::
  Members [Scratch, AtomicState Env, Stop YankError, Stop Report, Log, ChronosTime, Async, Input Ident] r =>
  Members [Settings !! SettingError, Settings, Rpc] r =>
  (Yank -> Sem r ()) ->
  Paste ->
  Sem r ()
repaste paster (Paste _ index _ _ visual) = do
  count <- length <$> yanks
  if count > 0
  then run count
  else stop YankError.EmptyHistory
  where
    run count = do
      undo
      reset
      insertPaste True paster (fromMaybe 0 (mod (index + 1) count))
    reset =
      when visual (normal "gv")

startPaste ::
  Members [Scratch, AtomicState Env, Stop YankError, Stop Report, Log, ChronosTime, Async, Input Ident] r =>
  Members [Settings !! SettingError, Settings, Rpc] r =>
  Maybe YankCommand ->
  (Yank -> Sem r ()) ->
  Sem r ()
startPaste command paster = do
  deleteYankScratch
  syncClipboard
  setCommand command
  insertPaste False paster 0

type PasteStack =
  [
    Scratch !! RpcError,
    Settings !! SettingError,
    Rpc !! RpcError,
    Lock @@ PasteLock,
    Resource,
    AtomicState Env,
    Log,
    ChronosTime,
    Async,
    Input Ident
  ]

pasteRequest ::
  Members PasteStack r =>
  Maybe YankCommand ->
  (Yank -> Sem (Lock : Stop YankError : Scratch : Settings : Rpc : Stop Report : r) ()) ->
  Handler r ()
pasteRequest commands paster =
  pluginLogReports $ mapReport @YankError $ tag $ lock do
    maybe (startPaste commands paster) (repaste paster) =<< currentPaste

uraPasteFor ::
  Members PasteStack r =>
  Maybe YankCommand ->
  Handler r ()
uraPasteFor commands =
  pasteRequest commands paste

uraPpasteFor ::
  Members PasteStack r =>
  Maybe YankCommand ->
  Handler r ()
uraPpasteFor commands =
  pasteRequest commands ppaste

uraPaste ::
  Members PasteStack r =>
  Handler r ()
uraPaste =
  pasteRequest Nothing paste

uraPpaste ::
  Members PasteStack r =>
  Handler r ()
uraPpaste =
  pasteRequest Nothing ppaste

uraStopPaste ::
  Members [Scratch !! RpcError, AtomicState Env, Lock @@ PasteLock, Resource, Log] r =>
  Handler r ()
uraStopPaste =
  resumeReport @Scratch $
  void (tag (lockOrSkip (whenM pasteActive cancelPaste)))
