module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident)
import qualified Data.Text as Text (length, take)
import Exon (exon)
import Ribosome (
  Handler,
  Report,
  Rpc,
  RpcError,
  ScratchId (ScratchId),
  SettingError,
  Settings,
  mapReport,
  resumeReport,
  )
import Ribosome.Api (vimGetCurrentWindow, windowGetWidth)
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import Ribosome.Menu (
  Mappings,
  MenuItem,
  MenuLoops,
  MenuWidget,
  NvimMenuUi,
  WindowMenu,
  simpleMenuItem,
  staticNvimMenu,
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
  MenuWidget Ident r YankAction
menuAction action = do
  withFocus (pure . action)

menuYank ::
  MenuWidget Ident r YankAction
menuYank =
  menuAction ActionLoad

menuPaste ::
  MenuWidget Ident r YankAction
menuPaste =
  menuAction ActionPaste

menuPpaste ::
  MenuWidget Ident r YankAction
menuPpaste =
  menuAction ActionPpaste

yankMenuItems :: Int -> [Yank] -> [MenuItem Ident]
yankMenuItems width yanks' =
  uncurry menuItem <$> zip yanks' [(0 :: Int)..]
  where
    menuItem (Yank ident _ _ _ (line' :| rest)) _ =
      simpleMenuItem ident (Text.take maxlen line' <> dots line' <> count rest)
    dots line' =
      if Text.length line' > maxlen then "..." else ""
    count [] =
      ""
    count ls =
      [exon| [#{show (length ls + 1)}]|]
    maxlen =
      width - 6

yankMenuMappings ::
  Mappings Ident r YankAction
yankMenuMappings =
  [("p", menuPaste), ("P", menuPpaste), ("y", menuYank)]

type YankMenuStack ui =
  [
    NvimMenuUi ui,
    MenuLoops Ident,
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
  result <- mapReport @RpcError $ staticNvimMenu (toList items) def scratchOptions yankMenuMappings
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
