module Uracil.YankScratch where

import qualified Control.Lens as Lens (view)
import Control.Lens ((?~), (^.))
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map.Strict as Map (fromList)
import qualified Data.Text as Text (length)
import Exon (exon)
import Ribosome (
  HandlerError,
  Rpc,
  Scratch,
  ScratchId,
  ScratchOptions,
  ScratchState,
  defaultScratchOptions,
  msgpackArray,
  )
import Ribosome.Api (currentCursor, setLine)
import qualified Ribosome.Float as Float
import Ribosome.Host.Api.Effect (vimCallFunction, windowSetOption)
import qualified Ribosome.Scratch as Scratch

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
        Float.relative = Float.Win,
        Float.width = width,
        Float.height = height,
        Float.bufpos = Just (row, col)
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
  windowSetOption (Scratch.window scratch) "cursorline" False
  windowSetOption (Scratch.window scratch) "signcolumn" ("no" :: Text)
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
