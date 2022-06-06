module Uracil.Paste where

import Chiasma.Data.Ident (Ident, generateIdent)
import qualified Chronos
import qualified Conc as Sync
import qualified Control.Lens as Lens (view)
import Control.Lens ((%~), (.~), (?~))
import Control.Monad.Extra (andM)
import Data.Generics.Labels ()
import Data.List (notElem)
import qualified Data.Text as Text (isInfixOf)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Polysemy.Time (Seconds)
import Ribosome.Api.Mode (visualModeActive)
import Ribosome.Api.Normal (noautocmdNormal, normal)
import Ribosome.Api.Register (getregList, getregtype, unnamedRegister)
import Ribosome.Api.Undo (undo)
import Ribosome.Api.Window (redraw)
import Ribosome.Data.Register (Register, registerRepr)
import qualified Ribosome.Data.Register as Register (Register (..))
import qualified Ribosome.Data.ScratchState as ScratchState
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host (Rpc, RpcError)
import Ribosome.Host.Api.Effect (vimGetOption)
import Ribosome.Host.Data.HandlerError (HandlerError, mapHandlerError, resumeHandlerError)
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Locks (lockOrSkip)
import qualified Time

import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import Uracil.Data.Paste (Paste (Paste))
import Uracil.Data.PasteLock (PasteLock (PasteLock))
import Uracil.Data.Yank (Yank)
import qualified Uracil.Data.Yank as Yank (content)
import Uracil.Data.YankCommand (YankCommand)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError (EmptyHistory))
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Yank (allYanks, loadYank, setCommand, storeYank, yankByIdent, yankByIndex, yanks)
import Uracil.YankScratch (ensureYankScratch, killYankScratch, selectYankInScratch)

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
  resume_ (noautocmdNormal (registerRepr register <> cmd))
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
  atomicGets Env.paste

pasteActive ::
  Member (AtomicState Env) r =>
  Sem r Bool
pasteActive =
  isJust <$> currentPaste

pasteHasTimedOut ::
  Member ChronosTime r =>
  Seconds ->
  Chronos.Time ->
  Sem r Bool
pasteHasTimedOut timeout updated = do
  diff <- Time.since updated
  pure (diff >= timeout)

shouldCancelPaste ::
  Members [AtomicState Env, ChronosTime] r =>
  Seconds ->
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
  killYankScratch
  setCommand Nothing

cancelPasteAfter ::
  Members [Scratch, AtomicState Env, ChronosTime, Log] r =>
  Seconds ->
  Ident ->
  Sem r ()
cancelPasteAfter timeout ident =
  whenM (shouldCancelPaste timeout ident) cancelPaste

waitAndCancelPaste ::
  Members [Scratch, AtomicState Env, ChronosTime, Log] r =>
  Member Settings r =>
  Ident ->
  Sem r ()
waitAndCancelPaste ident = do
  duration <- Settings.get Settings.pasteTimeout
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
  Members [Scratch, AtomicState Env, Stop YankError, Stop HandlerError, Log, ChronosTime, Async, Embed IO] r =>
  Members [Settings, Rpc] r =>
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
  ident <- generateIdent
  atomicModify' (#paste ?~ Paste ident index updated (ScratchState.id scratch) visual)
  selectYankInScratch scratch index
  redraw
  void (async (waitAndCancelPaste ident))

pullRegister ::
  Members [Rpc, AtomicState Env, Log, Embed IO] r =>
  Register ->
  NonEmpty Text ->
  Sem r ()
pullRegister register content = do
  tpe <- getregtype register
  storeYank tpe register "y" content

fetchClipboard ::
  Members [Rpc, AtomicState Env, Log, Embed IO] r =>
  [NonEmpty Text] ->
  Maybe (NonEmpty Text) ->
  Register ->
  Sem r ()
fetchClipboard lastTwoYanks skip reg =
  traverse_ check . nonEmpty =<< getregList reg
  where
    check content =
      when (freshYank content) (pullRegister reg content)
    freshYank a =
      a /= ("" :| []) && notElem a lastTwoYanks && notElem a skip

syncClipboard ::
  Members [Rpc, AtomicState Env, Log, Embed IO] r =>
  Sem r ()
syncClipboard = do
  lastTwoYanks <- take 2 . fmap (Lens.view Yank.content) <$> allYanks
  skip <- atomicGets Env.skip
  atomicModify' (#skip .~ Nothing)
  traverse_ @[] (fetchClipboard lastTwoYanks skip) [Register.Special "*", Register.Special "\""]

repaste ::
  Members [Scratch, AtomicState Env, Stop YankError, Stop HandlerError, Log, ChronosTime, Async, Embed IO] r =>
  Members [Settings, Rpc] r =>
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
  Members [Scratch, AtomicState Env, Stop YankError, Stop HandlerError, Log, ChronosTime, Async, Embed IO] r =>
  Members [Settings, Rpc] r =>
  Maybe YankCommand ->
  (Yank -> Sem r ()) ->
  Sem r ()
startPaste command paster =
  killYankScratch *>
  syncClipboard *>
  setCommand command *>
  insertPaste False paster 0

type PasteStack =
  [
    Scratch !! RpcError,
    Settings !! SettingError,
    Rpc !! RpcError,
    Sync PasteLock,
    Resource,
    AtomicState Env,
    Log,
    ChronosTime,
    Async,
    Embed IO
  ]

pasteRequest ::
  Members PasteStack r =>
  Maybe YankCommand ->
  (Yank -> Sem (Stop YankError : Scratch : Settings : Rpc : Stop HandlerError : r) ()) ->
  Handler r ()
pasteRequest commands paster =
  resumeHandlerError @Rpc $
  resumeHandlerError @Settings $
  resumeHandlerError @Scratch $
  mapHandlerError @YankError $
  Sync.lock PasteLock do
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
  Members [Scratch !! RpcError, AtomicState Env, Sync PasteLock, Resource, Log] r =>
  Handler r ()
uraStopPaste =
  resumeHandlerError @Scratch $
  void (lockOrSkip @PasteLock (whenM pasteActive cancelPaste))