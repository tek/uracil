{-# LANGUAGE QuasiQuotes #-}

module Uracil.Paste where

import Chiasma.Data.Ident (Ident, generateIdent)
import Control.Concurrent.Lifted (fork)
import qualified Control.Lens as Lens (view)
import Data.Composition ((.:))
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import Data.String.QM (qt)
import qualified Data.Text as Text (isInfixOf)
import Ribosome.Api.Mode (visualModeActive)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Register (getregList, getregtype)
import Ribosome.Api.Undo (undo)
import Ribosome.Api.Window (redraw)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Lock (lockOrWait)
import Ribosome.Data.Register (Register, registerRepr)
import qualified Ribosome.Data.Register as Register (Register(..))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetOption)
import System.Hourglass (timeCurrent)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (paste, yanks)
import Uracil.Data.Paste (Paste(Paste))
import Uracil.Data.Yank (Yank)
import qualified Uracil.Data.Yank as Yank (text)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory))
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Yank (loadYank, storeYank, yankByIdent, yankByIndex, yanks)
import Uracil.YankScratch (ensureYankScratch, killYankScratch, selectYankInScratch)

defaultRegister ::
  NvimE e m =>
  m Register
defaultRegister =
  Register.Special . decide <$> vimGetOption "clipboard"
  where
    decide "unnamed" =
      "*"
    decide a | "unnamedplus" `Text.isInfixOf` a =
      "+"
    decide _ =
      "\""

pasteWith ::
  MonadRibo m =>
  NvimE e m =>
  Text ->
  Yank ->
  m ()
pasteWith cmd yank = do
  register <- defaultRegister
  loadYank register yank
  normal (registerRepr register <> cmd)

paste ::
  MonadRibo m =>
  NvimE e m =>
  Yank ->
  m ()
paste =
  pasteWith "p"

ppaste ::
  MonadRibo m =>
  NvimE e m =>
  Yank ->
  m ()
ppaste =
  pasteWith "P"

pasteIdent ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m ()
pasteIdent =
  paste <=< yankByIdent

ppasteIdent ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m ()
ppasteIdent =
  ppaste <=< yankByIdent

currentPaste ::
  MonadDeepState s Env m =>
  m (Maybe Paste)
currentPaste =
  getL @Env Env.paste

pasteActive ::
  MonadDeepState s Env m =>
  m Bool
pasteActive =
  isJust <$> currentPaste

now ::
  MonadIO m =>
  m Elapsed
now =
  liftIO timeCurrent

pasteHasTimedOut ::
  MonadIO m =>
  Int ->
  Elapsed ->
  m Bool
pasteHasTimedOut timeout updated = do
  n <- now
  return $ (n - updated) >= Elapsed (Seconds (fromIntegral timeout))

shouldCancelPaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  Int ->
  Ident ->
  m Bool
shouldCancelPaste timeout ident =
  fromMaybe False <$$> traverse check =<< getL @Env Env.paste
  where
    check (Paste pasteIdent' _ updated _ _) =
      andM [pasteHasTimedOut timeout updated, pure $ ident == pasteIdent']

moveYankToHistoryHead ::
  MonadDeepState s Env m =>
  Int ->
  m ()
moveYankToHistoryHead index =
  modifyL @Env Env.yanks move
  where
    move ys =
      take 1 post <> pre <> drop 1 post
      where
        (pre, post) = splitAt index ys

movePastedToHistoryHead ::
  MonadDeepState s Env m =>
  m ()
movePastedToHistoryHead =
  traverse_ move =<< getL @Env Env.paste
  where
    move (Paste _ index _ _ _) =
      moveYankToHistoryHead index

cancelPaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m ()
cancelPaste =
  movePastedToHistoryHead *>
  setL @Env Env.paste Nothing *>
  killYankScratch

cancelPasteAfter ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  Int ->
  Ident ->
  m ()
cancelPasteAfter timeout ident =
  whenM (shouldCancelPaste timeout ident) cancelPaste

waitAndCancelPaste ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  Ident ->
  m ()
waitAndCancelPaste ident = do
  duration <- setting Settings.pasteTimeout
  sleep (fromIntegral duration)
  cancelPasteAfter duration ident

logUpdatePaste ::
  MonadRibo m =>
  Text ->
  Bool ->
  Text ->
  m ()
logUpdatePaste index visual yank =
  logDebug [qt|repasting with index ${index} in ${visualT} mode: ${yank} |]
  where
    visualT :: Text
    visualT = if visual then "visual" else "normal"

updatePaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  (Yank -> m ()) ->
  Int ->
  m ()
updatePaste paster index = do
  visual <- visualModeActive
  yank <- yankByIndex index
  logUpdatePaste (show (index + 1)) visual (show yank)
  paster yank
  scratch <- ensureYankScratch
  updated <- now
  ident <- generateIdent
  setL @Env Env.paste $ Just $ Paste ident index updated scratch visual
  selectYankInScratch scratch index
  redraw
  void $ fork (waitAndCancelPaste ident)

exclusiveUpdatePaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  (Yank -> m ()) ->
  Int ->
  m ()
exclusiveUpdatePaste =
  lockOrWait "uracil-update-paste" .: updatePaste

pullRegister ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Register ->
  NonEmpty Text ->
  m ()
pullRegister register content = do
  tpe <- getregtype register
  storeYank tpe register content

syncClipboard ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  m ()
syncClipboard = do
  currentYanks <- Lens.view Yank.text <$$> yanks
  traverse_ (fetch currentYanks) [Register.Special "*", Register.Special "\""]
  where
    fetch ys reg =
      traverse_ (check ys reg) =<< nonEmpty <$> getregList reg
    check ys reg content =
      when (freshYank ys content) (pullRegister reg content)
    freshYank ys a =
      a /= ("" :| []) && (a `notElem` ys)

repaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  (Yank -> m ()) ->
  Paste ->
  m ()
repaste paster (Paste _ index _ _ visual) = do
  count <- length <$> getL @Env Env.yanks
  if count > 0
  then run count
  else throwHoist YankError.EmptyHistory
  where
    run count =
      undo *>
      reset *>
      exclusiveUpdatePaste paster ((index + 1) `mod` count)
    reset =
      when visual $ normal "gv"

startPaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  (Yank -> m ()) ->
  m ()
startPaste paster =
  killYankScratch *>
  syncClipboard *>
  updatePaste paster 0

pasteRequest ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  (Yank -> m ()) ->
  m ()
pasteRequest paster =
  maybe (startPaste paster) (repaste paster) =<< currentPaste

uraPaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m ()
uraPaste =
  pasteRequest paste

uraPpaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m ()
uraPpaste =
  pasteRequest ppaste

uraStopPaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m ()
uraStopPaste =
  whenM pasteActive cancelPaste
