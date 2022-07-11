module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident)
import qualified Data.Text as Text (length, take)
import Exon (exon)
import Ribosome (
  Handler,
  HandlerError,
  Rpc,
  RpcError,
  SettingError,
  Settings,
  mapHandlerError,
  resumeHandlerError,
  )
import Ribosome.Api (vimGetCurrentWindow, windowGetWidth)
import Ribosome.Menu (
  Mappings,
  MenuItem,
  MenuState,
  MenuWidget,
  NvimMenu,
  menu,
  runStaticNvimMenu,
  simpleMenuItem,
  withFocus,
  withMappings,
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
  Member (MenuState Ident) r =>
  (Ident -> YankAction) ->
  MenuWidget r YankAction
menuAction action = do
  withFocus (pure . action)

menuYank ::
  Member (MenuState Ident) r =>
  MenuWidget r YankAction
menuYank =
  menuAction ActionLoad

menuPaste ::
  Member (MenuState Ident) r =>
  MenuWidget r YankAction
menuPaste =
  menuAction ActionPaste

menuPpaste ::
  Member (MenuState Ident) r =>
  MenuWidget r YankAction
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
  Member (MenuState Ident) r =>
  Mappings r YankAction
yankMenuMappings =
  [("p", menuPaste), ("P", menuPpaste), ("y", menuYank)]

type YankMenuStack =
  NvimMenu Ident ++ [
    Settings !! SettingError,
    AtomicState Env,
    Rpc !! RpcError
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
  Members YankMenuStack r =>
  Members [Rpc, Stop YankError, Stop HandlerError] r =>
  Maybe YankCommand ->
  Sem r ()
yankMenuWith operators = do
  width <- windowGetWidth =<< vimGetCurrentWindow
  items <- stopNote YankError.EmptyHistory . nonEmpty . yankMenuItems width =<< yanksFor operators
  result <- mapHandlerError @RpcError $ runStaticNvimMenu (toList items) [] def $ withMappings yankMenuMappings do
    menu
  handleResult "yank" yankAction result

uraYankMenuFor ::
  Members YankMenuStack r =>
  Maybe YankCommand ->
  Handler r ()
uraYankMenuFor operators = do
  resumeHandlerError @Rpc $ mapHandlerError @YankError do
    yankMenuWith operators

uraYankMenu ::
  Members YankMenuStack r =>
  Handler r ()
uraYankMenu =
  uraYankMenuFor Nothing
