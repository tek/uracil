module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident)
import Conc (Restoration)
import Control.Lens (use)
import qualified Data.Text as Text (length, take)
import Exon (exon)
import Polysemy.Chronos (ChronosTime)
import Ribosome (Handler, Rpc, RpcError, Scratch, SettingError, Settings, mapHandlerError, resumeHandlerError, HandlerError)
import Ribosome.Api (vimGetCurrentWindow, windowGetWidth)
import Ribosome.Menu (
  Mappings,
  MenuItem (MenuItem),
  MenuRead,
  MenuWidget,
  PromptConfig,
  PromptListening,
  defaultPrompt,
  focus,
  interpretMenu,
  menuRead,
  menuSuccess,
  semState,
  simpleMenuItem,
  staticNvimMenuDef,
  withMappings,
  )
import Ribosome.Menu.Data.MenuState (MenuStack)

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank (Yank))
import Uracil.Data.YankCommand (YankCommand)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError (EmptyHistory, InvalidMenuIndex))
import Uracil.Paste (pasteIdent, ppasteIdent)
import Uracil.Yank (loadYankIdent, yanksFor)
import Uracil.Menu (handleResult)

data YankAction =
  ActionLoad Ident
  |
  ActionPaste Ident
  |
  ActionPpaste Ident
  deriving stock (Eq, Show)

menuAction ::
  MenuRead Ident r =>
  Member (Stop YankError) r =>
  (Ident -> YankAction) ->
  MenuWidget r YankAction
menuAction action = do
  MenuItem ident _ _ <- stopNote YankError.InvalidMenuIndex =<< menuRead (semState (use focus))
  menuSuccess (action ident)

menuYank ::
  MenuRead Ident r =>
  Member (Stop YankError) r =>
  MenuWidget r YankAction
menuYank =
  menuAction ActionLoad

menuPaste ::
  MenuRead Ident r =>
  Member (Stop YankError) r =>
  MenuWidget r YankAction
menuPaste =
  menuAction ActionPaste

menuPpaste ::
  MenuRead Ident r =>
  Member (Stop YankError) r =>
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
  MenuRead Ident r =>
  Members [AtomicState Env, Rpc !! e, Rpc, Log, Stop YankError, Resource, Embed IO] r =>
  Mappings r YankAction
yankMenuMappings =
  [("p", menuPaste), ("P", menuPpaste), ("y", menuYank)]

type YankMenuStack =
  [
    Scratch !! RpcError,
    Settings !! SettingError,
    Sync PromptListening,
    Race,
    Mask Restoration,
    AtomicState Env,
    Rpc !! RpcError,
    Log,
    ChronosTime,
    Resource,
    Embed IO,
    Final IO
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
  Members (Sync PromptListening : MenuStack Ident) r =>
  Members [Rpc, Scratch, Stop YankError, Stop HandlerError] r =>
  PromptConfig ->
  Maybe YankCommand ->
  Sem r ()
yankMenuWith promptConfig operators = do
  width <- windowGetWidth =<< vimGetCurrentWindow
  items <- stopNote YankError.EmptyHistory . nonEmpty . yankMenuItems width =<< yanksFor operators
  withMappings yankMenuMappings do
    result <- staticNvimMenuDef def (toList items) promptConfig
    handleResult "yank" yankAction result

uraYankMenuFor ::
  Members YankMenuStack r =>
  Maybe YankCommand ->
  Handler r ()
uraYankMenuFor operators = do
  resumeHandlerError @Rpc $ resumeHandlerError @Scratch $ mapHandlerError @YankError do
    promptConfig <- defaultPrompt []
    interpretMenu $ yankMenuWith promptConfig operators

uraYankMenu ::
  Members YankMenuStack r =>
  Handler r ()
uraYankMenu =
  uraYankMenuFor Nothing
