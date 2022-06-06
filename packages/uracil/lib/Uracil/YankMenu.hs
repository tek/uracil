module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident)
import Control.Lens (use)
import qualified Data.Map.Strict as Map (fromList)
import qualified Data.Text as Text (length, take)
import Polysemy.Chronos (ChronosTime)
import Ribosome (Handler, Rpc, RpcError, Scratch, SettingError, Settings, mapHandlerError, resumeHandlerError)
import Ribosome.Api (vimGetCurrentWindow, windowGetWidth)
import qualified Ribosome.Menu as Menu
import Ribosome.Menu (
  Mappings,
  MenuItem (MenuItem),
  MenuWidget,
  PromptListening,
  defaultPrompt,
  focus,
  fuzzyItemFilter,
  menuRead,
  menuSuccess,
  semState,
  simpleMenuItem,
  staticNvimMenuWith,
  )

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank (Yank))
import Uracil.Data.YankCommand (YankCommand)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError (EmptyHistory, InvalidMenuIndex))
import Uracil.Paste (pasteIdent, ppasteIdent)
import Uracil.Yank (loadYankIdent, yanksFor)
import Exon (exon)

menuAction ::
  Members [Stop YankError, Resource, Embed IO] r =>
  (Ident -> Sem r ()) ->
  MenuWidget Ident r ()
menuAction action = do
  MenuItem ident _ _ <- stopNote YankError.InvalidMenuIndex =<< menuRead (semState (use focus))
  menuSuccess (action ident)

menuYank ::
  Members [Rpc, Log, AtomicState Env, Stop YankError, Resource, Embed IO] r =>
  MenuWidget Ident r ()
menuYank =
  menuAction loadYankIdent

menuPaste ::
  Members [AtomicState Env, Rpc !! e, Rpc, Log, Stop YankError, Resource, Embed IO] r =>
  MenuWidget Ident r ()
menuPaste =
  menuAction pasteIdent

menuPpaste ::
  Members [AtomicState Env, Rpc !! e, Rpc, Log, Stop YankError, Resource, Embed IO] r =>
  MenuWidget Ident r ()
menuPpaste =
  menuAction ppasteIdent

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
  Members [AtomicState Env, Rpc !! e, Rpc, Log, Stop YankError, Resource, Embed IO] r =>
  Mappings Ident r ()
yankMenuMappings =
  Map.fromList [("p", menuPaste), ("P", menuPpaste), ("y", menuYank)]

type YankMenuStack res =
  [
    Scratch !! RpcError,
    Settings !! SettingError,
    Sync PromptListening,
    Race,
    Mask res,
    AtomicState Env,
    Rpc !! RpcError,
    Log,
    ChronosTime,
    Resource,
    Embed IO,
    Final IO
  ]

uraYankMenuFor ::
  Members (YankMenuStack res) r =>
  Maybe YankCommand ->
  Handler r ()
uraYankMenuFor operators = do
  resumeHandlerError @Rpc $ resumeHandlerError @Scratch $ mapHandlerError @YankError do
    promptConfig <- defaultPrompt []
    width <- windowGetWidth =<< vimGetCurrentWindow
    items <- stopNote YankError.EmptyHistory . nonEmpty . yankMenuItems width =<< yanksFor operators
    void (staticNvimMenuWith (fuzzyItemFilter False) def (toList items) consumer promptConfig)
  where
    consumer =
      Menu.withMappings yankMenuMappings

uraYankMenu ::
  Members (YankMenuStack res) r =>
  Handler r ()
uraYankMenu =
  uraYankMenuFor Nothing
