{-# LANGUAGE QuasiQuotes #-}

module Uracil.Paste where

import Chiasma.Data.Ident (Ident, generateIdent)
import Control.Concurrent.Lifted (fork)
import qualified Control.Lens as Lens (view)
import Data.Foldable (maximum)
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.String.QM (qt)
import qualified Data.Text as Text (isInfixOf, length)
import Ribosome.Api.Mode (visualModeActive)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Register (getregList, getregtype)
import Ribosome.Api.Undo (undo)
import Ribosome.Api.Window (redraw, setLine)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Lock (lockOrWait)
import qualified Ribosome.Data.FloatOptions as FloatOptions (FloatOptions(height, width))
import Ribosome.Data.Register (Register, registerRepr)
import qualified Ribosome.Data.Register as Register (Register(..))
import Ribosome.Data.Scratch (Scratch(Scratch, scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions, defaultScratchOptions, scratchFloat, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.Syntax (HiLink(HiLink), Syntax(Syntax), syntaxMatch)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetOption, windowSetOption)
import Ribosome.Scratch (killScratchByName, showInScratch)
import System.Hourglass (timeCurrent)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (paste, previousStar, yanks)
import Uracil.Data.Paste (Paste(Paste))
import qualified Uracil.Data.Paste as Paste (scratch)
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

cancelPaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m ()
cancelPaste =
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
  Int ->
  m ()
updatePaste index = do
  visual <- visualModeActive
  yank <- yankByIndex index
  logUpdatePaste (show (index + 1)) visual (show yank)
  paste yank
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
  Int ->
  m ()
exclusiveUpdatePaste =
  lockOrWait "uracil-update-paste" . updatePaste

pullStarRegister ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  [Text] ->
  m ()
pullStarRegister content = do
  setL @Env Env.previousStar content
  tpe <- getregtype register
  storeYank tpe register content
  where
    register =
      Register.Special "*"

syncClipboard ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  m ()
syncClipboard = do
  regStar <- getregList (Register.Special "*")
  regUnnamed <- getregList (Register.Special "\"")
  previousStar <- getL @Env Env.previousStar
  when (regStar /= [""] && regStar /= [] && regStar /= regUnnamed && previousStar /= regStar) (pullStarRegister regStar)

repaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  Paste ->
  m ()
repaste (Paste _ index _ _ visual) = do
  count <- length <$> getL @Env Env.yanks
  if count > 0
  then run count
  else throwHoist YankError.EmptyHistory
  where
    run count =
      undo *>
      reset *>
      exclusiveUpdatePaste ((index + 1) `mod` count)
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
  m ()
startPaste =
  killYankScratch *>
  syncClipboard *>
  updatePaste 0

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
  maybe startPaste repaste =<< currentPaste

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
