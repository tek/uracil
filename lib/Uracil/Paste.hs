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
import Ribosome.Api.Undo (undo)
import Ribosome.Api.Window (redraw, setLine)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Lock (lockOrWait)
import qualified Ribosome.Data.FloatOptions as FloatOptions (FloatOptions(height, width))
import Ribosome.Data.Scratch (Scratch(Scratch))
import Ribosome.Data.ScratchOptions (ScratchOptions, defaultScratchOptions)
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (float)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCommand, vimGetOption)
import Ribosome.Scratch (killScratchByName, showInScratch)
import System.Hourglass (timeCurrent)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (paste, yanks)
import Uracil.Data.Paste (Paste(Paste))
import qualified Uracil.Data.Paste as Paste (scratch)
import Uracil.Data.Yank (Yank)
import qualified Uracil.Data.Yank as Yank (text)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory))
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Yank (loadYank, yankByIdent, yankByIndex, yanks)

defaultRegister ::
  NvimE e m =>
  m Text
defaultRegister =
  decide <$> vimGetOption "clipboard"
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
  normal ("\"" <> register <> cmd)

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

yankLines ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  m (NonEmpty Text)
yankLines = do
  lines' <- formatLine . Lens.view Yank.text <$$> yanks
  hoistMaybe YankError.EmptyHistory (nonEmpty lines')
  where
    formatLine (h :| t) | null t =
      h
    formatLine (h :| t) =
      h <> [qt| [${len}]|]
      where
        len :: Text
        len = show (length t)

yankScratchName :: Text
yankScratchName =
  "uracil-yanks"

yankScratchOptions :: NonEmpty Text -> ScratchOptions
yankScratchOptions lines' =
  (defaultScratchOptions yankScratchName) { ScratchOptions.float = Just floatOptions }
  where
    floatOptions =
      def { FloatOptions.width = width, FloatOptions.height = height }
    width =
      min 40 (maximum (Text.length <$> lines')) + 5
    height =
      max 1 $ min 10 (length lines')

showYankScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m Scratch
showYankScratch = do
  lines' <- yankLines
  (`showInScratch` yankScratchOptions lines') (NonEmpty.toList lines')

selectYankInScratch ::
  NvimE e m =>
  Scratch ->
  Int ->
  m ()
selectYankInScratch (Scratch _ _ window _ _) =
  setLine window

ensureYankScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m Scratch
ensureYankScratch =
  maybe showYankScratch (pure . Lens.view Paste.scratch) =<< getL @Env Env.paste

now ::
  MonadIO m =>
  m Elapsed
now =
  liftIO timeCurrent

killYankScratch ::
  NvimE e m =>
  MonadRibo m =>
  m ()
killYankScratch =
  killScratchByName yankScratchName

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
cancelPasteAfter timeout ident = do
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
      when visual $ vimCommand "normal! gv"

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
