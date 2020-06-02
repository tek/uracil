{-# LANGUAGE QuasiQuotes #-}

module Uracil.YankScratch where

import qualified Control.Lens as Lens (view)
import Data.Foldable (maximum)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map.Strict as Map (fromList)
import Data.String.QM (qt)
import qualified Data.Text as Text (length)
import Ribosome.Api.Window (currentCursor, setLine)
import qualified Ribosome.Data.FloatOptions as FloatOptions
import Ribosome.Data.Scratch (Scratch(Scratch, scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions, defaultScratchOptions, scratchFloat)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, windowSetOption)
import Ribosome.Scratch (killScratchByName, showInScratch)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (paste)
import qualified Uracil.Data.Paste as Paste (scratch)
import qualified Uracil.Data.Yank as Yank (text)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory))
import Uracil.Yank (yanks)

yankScratchName :: Text
yankScratchName =
  "uracil-yanks"

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

signName :: Text
signName =
  "uracil-yanks-selected"

defineSignOptions :: Map Text Text
defineSignOptions =
  Map.fromList [("linehl", "PmenuSel"), ("texthl", "PmenuSel")]

defineSign ::
  NvimE e m =>
  m ()
defineSign =
  vimCallFunction "sign_define" [toMsgpack signName, toMsgpack defineSignOptions]

unplaceSignOptions :: Map Text Text
unplaceSignOptions =
  def

unplaceSign ::
  NvimE e m =>
  m ()
unplaceSign =
  vimCallFunction "sign_unplace" [toMsgpack signName, toMsgpack unplaceSignOptions]

placeSignOptions :: Int -> Map Text Int
placeSignOptions line =
  Map.fromList [("lnum", line + 1)]

placeSign ::
  NvimE e m =>
  Int ->
  m ()
placeSign line =
  vimCallFunction "sign_place" [
    toMsgpack (1 :: Int),
    toMsgpack signName,
    toMsgpack signName,
    toMsgpack yankScratchName,
    toMsgpack (placeSignOptions line)
    ]

moveSign ::
  NvimE e m =>
  Int ->
  m ()
moveSign line =
  unplaceSign *> placeSign line

yankScratchOptions :: NonEmpty Text -> Int -> Int -> ScratchOptions
yankScratchOptions lines' row col =
  scratchFloat float . defaultScratchOptions $ yankScratchName
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
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m Scratch
showYankScratch = do
  lines' <- yankLines
  (row, col) <- currentCursor
  scratch <- showInScratch (NonEmpty.toList lines') (yankScratchOptions lines' row col)
  windowSetOption (scratchWindow scratch) "cursorline" (toMsgpack False)
  windowSetOption (scratchWindow scratch) "signcolumn" (toMsgpack ("no" :: Text))
  defineSign
  return scratch

selectYankInScratch ::
  NvimE e m =>
  Scratch ->
  Int ->
  m ()
selectYankInScratch (Scratch _ _ window _ _) line =
  setLine window line *> moveSign line

ensureYankScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e YankError m =>
  MonadDeepState s Env m =>
  m Scratch
ensureYankScratch =
  maybe showYankScratch (pure . Lens.view Paste.scratch) =<< getL @Env Env.paste

killYankScratch ::
  NvimE e m =>
  MonadRibo m =>
  m ()
killYankScratch =
  killScratchByName yankScratchName
