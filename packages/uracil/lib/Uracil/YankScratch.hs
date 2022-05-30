module Uracil.YankScratch where

import qualified Control.Lens as Lens (view)
import Control.Lens ((?~), (^.))
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map.Strict as Map (fromList)
import qualified Data.Text as Text (length)
import Exon (exon)
import Ribosome.Api.Window (currentCursor, setLine)
import qualified Ribosome.Data.FloatOptions as FloatOptions
import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions, defaultScratchOptions)
import qualified Ribosome.Data.ScratchState as ScratchState
import Ribosome.Data.ScratchState (ScratchState)
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Host (Rpc)
import Ribosome.Host.Api.Effect (vimCallFunction, windowSetOption)
import Ribosome.Host.Class.Msgpack.Array (MsgpackArray (msgpackArray))
import Ribosome.Host.Data.HandlerError (HandlerError)

import qualified Uracil.Data.Env as Env (paste)
import Uracil.Data.Env (Env)
import qualified Uracil.Data.Paste as Paste
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
  lines' <- fmap (formatLine . Lens.view Yank.content) <$> yanks
  stopNote YankError.EmptyHistory (nonEmpty lines')
  where
    formatLine (h :| t) | null t =
      h
    formatLine (h :| t) =
      h <> [exon| [#{len}]|]
      where
        len :: Text
        len = show (length t)

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

yankScratchOptions :: NonEmpty Text -> Int -> Int -> ScratchOptions
yankScratchOptions lines' row col =
  defaultScratchOptions scratchId & #float ?~ float
  where
    float =
      def {
        FloatOptions.relative = FloatOptions.Win,
        FloatOptions.width = width,
        FloatOptions.height = height,
        FloatOptions.bufpos = Just (row, col)
      }
    width =
      min 40 (maximum (Text.length <$> lines')) + 5
    height =
      max 1 $ min 10 (length lines')

showYankScratch ::
  Members [Rpc, Scratch, AtomicState Env, Stop YankError, Stop HandlerError] r =>
  Sem r ScratchState
showYankScratch = do
  lines' <- yankLines
  (row, col) <- currentCursor
  scratch <- Scratch.show (NonEmpty.toList lines') (yankScratchOptions lines' row col)
  windowSetOption (ScratchState.window scratch) "cursorline" False
  windowSetOption (ScratchState.window scratch) "signcolumn" ("no" :: Text)
  scratch <$ defineSign

selectYankInScratch ::
  Member Rpc r =>
  ScratchState ->
  Int ->
  Sem r ()
selectYankInScratch scratch line =
  setLine (scratch ^. #window) line *> moveSign line

ensureYankScratch ::
  Members [Rpc, Scratch, AtomicState Env, Stop YankError, Stop HandlerError] r =>
  Sem r ScratchState
ensureYankScratch = do
  existing <- fmap join . traverse (Scratch.find . Paste.scratch) =<< atomicGets Env.paste
  maybe showYankScratch pure existing

killYankScratch ::
  Member Scratch r =>
  Sem r ()
killYankScratch =
  Scratch.kill scratchId
