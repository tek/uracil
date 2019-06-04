module Uracil.Paste where

import Chiasma.Data.Ident (Ident)
import Control.Concurrent.Lifted (fork)
import qualified Control.Lens as Lens (view)
import Control.Monad.DeepState (modifyML')
import Data.Foldable (maximum)
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import qualified Data.List.NonEmpty as NonEmpty (head, length, toList)
import qualified Data.Text as Text (length)
import Ribosome.Api.Undo (undo)
import Ribosome.Api.Window (redraw, setLine)
import Ribosome.Config.Setting (setting)
import qualified Ribosome.Data.FloatOptions as FloatOptions (FloatOptions(height, width))
import Ribosome.Data.Scratch (Scratch(Scratch))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions), defaultScratchOptions)
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (float)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import Ribosome.Scratch (killScratchByName, showInScratch)
import System.Hourglass (timeCurrent)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (paste, yanks)
import Uracil.Data.Paste (Paste(Paste))
import qualified Uracil.Data.Paste as Paste (scratch)
import Uracil.Data.Yank (Yank(Yank))
import qualified Uracil.Data.Yank as Yank (text)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory))
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Yank (loadYank, yankByIdent, yankByIndex, yanks)

pasteWith ::
  MonadRibo m =>
  NvimE e m =>
  Text ->
  Yank ->
  m ()
pasteWith cmd yank =
  loadYank "\"" yank *>
  vimCommand ("normal! " <> cmd)

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
  Paste ->
  m (Maybe Paste)
cancelIfElapsed timeout p@(Paste _ updated _) = do
  n <- now
  return $ if (n - updated) >= Elapsed (Seconds (fromIntegral timeout)) then Nothing else Just p

cancelPasteAfter ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  Int ->
  m ()
cancelPasteAfter timeout = do
  canceled <- isNothing <$> modifyML' @Env Env.paste (join <$$> traverse (cancelIfElapsed timeout))
  when canceled (killScratchByName yankScratchName)

waitAndCancelPaste ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  m ()
waitAndCancelPaste = do
  duration <- setting Settings.pasteTimeout
  sleep (fromIntegral duration)
  cancelPasteAfter duration

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
  scratch <- ensureYankScratch
  updated <- now
  setL @Env Env.paste $ Just $ Paste index updated scratch
  selectYankInScratch scratch index
  redraw
  yank <- yankByIndex index
  paste yank
  void $ fork waitAndCancelPaste

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
repaste (Paste index _ _) = do
  count <- length <$> getL @Env Env.yanks
  if count > 0
  then undo *> updatePaste ((index + 1) `mod` count)
  else throwHoist YankError.EmptyHistory

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
