module Uracil.Paste where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Ident (Ident, generateIdent, sameIdent)
import Control.Concurrent.Lifted (fork)
import qualified Control.Lens as Lens (view)
import Control.Monad.DeepState (modifyML')
import Data.Foldable (maximum)
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import qualified Data.List.NonEmpty as NonEmpty (head, toList)
import qualified Data.Text as Text (isInfixOf, length)
import Ribosome.Api.Mode (visualModeActive)
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
  vimCommand ("normal! \"" <> register <> cmd)

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

yankLines ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  m (NonEmpty Text)
yankLines = do
  lines' <- NonEmpty.head . Lens.view Yank.text <$$> yanks
  hoistMaybe YankError.EmptyHistory (nonEmpty lines')

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

cancelIfElapsed ::
  NvimE e m =>
  MonadRibo m =>
  Int ->
  Ident ->
  Paste ->
  m (Maybe Paste)
cancelIfElapsed timeout ident p@(Paste pasteIdent _ updated _ _) = do
  n <- now
  return $ if ident == pasteIdent && (n - updated) >= Elapsed (Seconds (fromIntegral timeout)) then Nothing else Just p

killYankScratch ::
  NvimE e m =>
  MonadRibo m =>
  m ()
killYankScratch =
  killScratchByName yankScratchName

cancelPasteAfter ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  Int ->
  Ident ->
  m ()
cancelPasteAfter timeout ident = do
  canceled <- isNothing <$> modifyML' @Env Env.paste (join <$$> traverse (cancelIfElapsed timeout ident))
  when canceled killYankScratch

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
  logDebug @Text ("repasting with index " <> show (index + 1) <> ": " <> show yank)
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
repaste (Paste ident index _ _ visual) = do
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
