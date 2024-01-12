module Uracil.YankScratch where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import Ribosome (Report, Rpc, Scratch, ScratchId, ScratchOptions, ScratchState, msgpackArray, scratch)
import Ribosome.Api (nvimGetCurrentWin, nvimWinGetWidth, setLine, vimCallFunction, windowSetOption)
import Ribosome.Data.FloatOptions (FloatAnchor (NE))
import qualified Ribosome.Float as Float
import qualified Ribosome.Scratch as Scratch

import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import qualified Uracil.Data.Paste as Paste
import Uracil.Data.Paste (Paste (Paste))
import qualified Uracil.Data.Yank as Yank (content)
import qualified Uracil.Data.YankError as YankError (YankError (EmptyHistory))
import Uracil.Data.YankError (YankError)
import Uracil.Yank (yanks)

scratchId :: ScratchId
scratchId =
  "uracil-yanks"

yankLines ::
  Members [AtomicState Env, Stop YankError] r =>
  Sem r (NonEmpty Text)
yankLines = do
  lines' <- fmap (formatLine . (.content)) <$> yanks
  stopNote YankError.EmptyHistory (nonEmpty lines')
  where
    formatLine = \case
      [h] ->
        h
      h :| t ->
        [exon|#{h} [#{show (length t)}]|]

signName :: Text
signName =
  "uracil-yanks-selected"

defineSignOptions :: Map Text Text
defineSignOptions =
  Map.fromList [("linehl", "PmenuSel"), ("texthl", "PmenuSel")]

defineSign ::
  Member Rpc r =>
  Sem r ()
defineSign =
  vimCallFunction "sign_define" (msgpackArray signName defineSignOptions)

unplaceSignOptions :: Map Text Text
unplaceSignOptions =
  def

unplaceSign ::
  Member Rpc r =>
  Sem r ()
unplaceSign =
  vimCallFunction "sign_unplace" (msgpackArray signName unplaceSignOptions)

placeSignOptions :: Int -> Map Text Int
placeSignOptions line =
  Map.fromList [("lnum", line + 1)]

placeSign ::
  Member Rpc r =>
  Int ->
  Sem r ()
placeSign line =
  vimCallFunction "sign_place" (msgpackArray (1 :: Int) signName signName scratchId (placeSignOptions line))

moveSign ::
  Member Rpc r =>
  Int ->
  Sem r ()
moveSign line =
  unplaceSign *> placeSign line

yankScratchOptions :: Int -> NonEmpty Text -> ScratchOptions
yankScratchOptions winwidth lines' =
  scratch scratchId & #float ?~ float
  where
    float =
      def {
        Float.relative = Float.Win,
        Float.anchor = NE,
        Float.width = width,
        Float.height = height,
        Float.row = 2,
        Float.col = winwidth - 2
      }
    width =
      min (fromMaybe 40 (div winwidth 2)) (fromMaybe 0 (maximum (Text.length <$> lines'))) + 5
    height =
      max 1 $ min 10 (length lines')

showYankScratch ::
  Members [Rpc, Scratch, AtomicState Env, Stop YankError, Stop Report] r =>
  Sem r ScratchState
showYankScratch = do
  lines' <- yankLines
  win <- nvimGetCurrentWin
  winwidth <- nvimWinGetWidth win
  scr <- Scratch.show (NonEmpty.toList lines') (yankScratchOptions winwidth lines')
  windowSetOption scr.window "cursorline" False
  windowSetOption scr.window "signcolumn" ("no" :: Text)
  scr <$ defineSign

selectYankInScratch ::
  Member Rpc r =>
  Int ->
  ScratchState ->
  Sem r ()
selectYankInScratch line scr =
  setLine (scr ^. #window) line *> moveSign line

ensureYankScratch ::
  Members [Rpc, Scratch, AtomicState Env, Stop YankError, Stop Report] r =>
  Sem r (Maybe ScratchState)
ensureYankScratch =
  atomicGets (.paste) >>= \case
    Just Paste {scratch = Just prev} -> do
      check =<< Scratch.find prev
    Just Paste {scratch = Nothing} ->
      pure Nothing
    Nothing ->
      Just <$> showYankScratch
  where
    check = fmap Just . fromMaybeA showYankScratch

deleteYankScratch ::
  Member Scratch r =>
  Sem r ()
deleteYankScratch =
  Scratch.delete scratchId
