{-# LANGUAGE QuasiQuotes #-}

module Uracil.YankScratch where

import Chiasma.Data.Ident (Ident, generateIdent)
import Control.Concurrent.Lifted (fork)
import qualified Control.Lens as Lens (view)
import Data.Foldable (maximum)
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map.Strict as Map (fromList)
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
import Ribosome.Data.ScratchOptions (ScratchOptions, defaultScratchOptions, scratchFloat)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.Syntax (HiLink(HiLink), Syntax(Syntax), syntaxMatch)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimGetOption, windowSetOption)
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

yankScratchOptions :: NonEmpty Text -> ScratchOptions
yankScratchOptions lines' =
  scratchFloat float . defaultScratchOptions $ yankScratchName
  where
    float =
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
  scratch <- showInScratch (NonEmpty.toList lines') (yankScratchOptions lines')
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
