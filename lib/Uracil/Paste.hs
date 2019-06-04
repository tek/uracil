module Uracil.Paste where

import Chiasma.Data.Ident (Ident)
import qualified Control.Lens as Lens (view)
import Control.Monad.DeepState (modifyML)
import Data.Hourglass (Elapsed)
import qualified Data.List.NonEmpty as NonEmpty (head)
import Ribosome.Api.Undo (undo)
import Ribosome.Api.Window (redraw)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import System.Hourglass (timeCurrent)

import Ribosome.Api.Window (setLine)
import Ribosome.Data.Scratch (Scratch(Scratch))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions), defaultScratchOptions)
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (float)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)
import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (paste, yanks)
import Uracil.Data.Paste (Paste(Paste))
import qualified Uracil.Data.Paste as Paste (scratch)
import Uracil.Data.Yank (Yank(Yank))
import qualified Uracil.Data.Yank as Yank (text)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory))
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
  m [Text]
yankLines =
  NonEmpty.head . Lens.view Yank.text <$$> yanks

yankScratchOptions :: ScratchOptions
yankScratchOptions =
  (defaultScratchOptions "uracil-yanks") { ScratchOptions.float = Just def }

showYankScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  m Scratch
showYankScratch =
  (`showInScratch` yankScratchOptions) =<< yankLines

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
  MonadDeepState s Env m =>
  m Scratch
ensureYankScratch =
  maybe showYankScratch (pure . Lens.view Paste.scratch) =<< getL @Env Env.paste

now ::
  MonadIO m =>
  m Elapsed
now =
  liftIO timeCurrent

updatePaste ::
  NvimE e m =>
  MonadRibo m =>
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

repaste ::
  NvimE e m =>
  MonadRibo m =>
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
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m ()
startPaste =
  updatePaste 0

uraPaste ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m ()
uraPaste =
  maybe startPaste repaste =<< currentPaste
