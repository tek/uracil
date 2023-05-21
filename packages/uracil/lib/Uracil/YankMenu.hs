module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident)
import Data.Char (isSpace)
import qualified Data.Text as Text
import Exon (exon)
import Ribosome (Handler, Report, Rpc, RpcError, ScratchId (ScratchId), SettingError, Settings, mapReport, resumeReport)
import Ribosome.Api (vimGetCurrentWindow, windowGetWidth)
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import Ribosome.Menu (
  Filter (Fuzzy),
  Mappings,
  MenuItem,
  MenuWidget,
  ModalState,
  ModalWindowMenus,
  WindowMenu,
  modal,
  simpleMenuItem,
  staticWindowMenu,
  withFocus,
  )

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank (Yank))
import Uracil.Data.YankCommand (YankCommand)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError (EmptyHistory))
import Uracil.Menu (handleResult)
import Uracil.Paste (pasteIdent, ppasteIdent)
import Uracil.Yank (loadYankIdent, yanksFor)

data YankAction =
  ActionLoad Ident
  |
  ActionPaste Ident
  |
  ActionPpaste Ident
  deriving stock (Eq, Show)

menuAction ::
  (Ident -> YankAction) ->
  MenuWidget (ModalState Ident) r YankAction
menuAction action = do
  withFocus (pure . action)

menuYank ::
  MenuWidget (ModalState Ident) r YankAction
menuYank =
  menuAction ActionLoad

menuPaste ::
  MenuWidget (ModalState Ident) r YankAction
menuPaste =
  menuAction ActionPaste

menuPpaste ::
  MenuWidget (ModalState Ident) r YankAction
menuPpaste =
  menuAction ActionPpaste

menuItem :: Int -> Yank -> p -> MenuItem Ident
menuItem width (Yank ident _ _ _ ls) _ =
  simpleMenuItem ident (Text.take maxlen display <> dots <> count)
  where
    dots = if Text.length display > maxlen then "..." else ""
    count | len <= 1 = ""
          | otherwise = [exon| [#{show len}]|]
    len = length ls
    maxlen = width - 9
    display = fromMaybe "<whitespace>" (find displayable ls)
    displayable l = not (Text.null l) && Text.any (not . isSpace) l

yankMenuItems :: Int -> [Yank] -> [MenuItem Ident]
yankMenuItems width yanks' =
  uncurry (menuItem width) <$> zip yanks' [(0 :: Int)..]

yankMenuMappings :: Mappings (ModalState Ident) r YankAction
yankMenuMappings =
  [("p", menuPaste), ("P", menuPpaste), ("y", menuYank)]

type YankMenuStack ui =
  [
    ModalWindowMenus Ident !! RpcError,
    Settings !! SettingError,
    AtomicState Env,
    Rpc !! RpcError,
    Log
  ]

yankAction ::
  Members [AtomicState Env, Stop YankError, Rpc, Log, Rpc !! RpcError] r =>
  YankAction ->
  Sem r ()
yankAction = \case
  ActionLoad i -> loadYankIdent i
  ActionPaste i -> pasteIdent i
  ActionPpaste i -> ppasteIdent i

yankMenuWith ::
  Members (YankMenuStack ui) r =>
  Members [Rpc, Stop YankError, Stop Report] r =>
  Maybe YankCommand ->
  Sem r ()
yankMenuWith operators = do
  width <- windowGetWidth =<< vimGetCurrentWindow
  items <- stopNote YankError.EmptyHistory . nonEmpty . yankMenuItems width =<< yanksFor operators
  result <- mapReport @RpcError $ staticWindowMenu (toList items) (modal Fuzzy) (def & #items .~ scratchOptions) yankMenuMappings
  handleResult yankAction result
  where
    scratchOptions =
      def {
        name = ScratchId name,
        filetype = Just name
      }
    name =
      "uracil-yanks"

uraYankMenuFor ::
  Members (YankMenuStack WindowMenu) r =>
  Maybe YankCommand ->
  Handler r ()
uraYankMenuFor operators = do
  resumeReport @Rpc $ mapReport @YankError do
    yankMenuWith operators

uraYankMenu ::
  Members (YankMenuStack WindowMenu) r =>
  Handler r ()
uraYankMenu =
  uraYankMenuFor Nothing
