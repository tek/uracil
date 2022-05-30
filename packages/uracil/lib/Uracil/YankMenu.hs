module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident)
import Conc (interpretSync)
import Control.Lens (use)
import qualified Data.Map.Strict as Map (fromList)
import qualified Data.Text as Text (length, take)
import Polysemy.Chronos (ChronosTime)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host (Rpc, RpcError)
import Ribosome.Host.Api.Effect (vimGetCurrentWindow, windowGetWidth)
import Ribosome.Host.Data.HandlerError (mapHandlerError, resumeHandlerError)
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Menu.Action (menuSuccess)
import qualified Ribosome.Menu.Consumer as Consumer
import Ribosome.Menu.Consumer (Mappings)
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem), simpleMenuItem)
import Ribosome.Menu.Data.MenuState (menuRead, semState)
import Ribosome.Menu.Filters (fuzzyItemFilter)
import Ribosome.Menu.ItemLens (focus)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Run (staticNvimMenuWith)

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank (Yank))
import Uracil.Data.YankCommand (YankCommand)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError (EmptyHistory, InvalidMenuIndex))
import Uracil.Paste (pasteIdent, ppasteIdent)
import Uracil.Yank (loadYankIdent, yanksFor)

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
      " [" <> show (length ls + 1) <> "]"
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
  resumeHandlerError @Rpc $ resumeHandlerError @Scratch $ mapHandlerError @YankError $ interpretSync do
    promptConfig <- defaultPrompt []
    width <- windowGetWidth =<< vimGetCurrentWindow
    items <- stopNote YankError.EmptyHistory . nonEmpty . yankMenuItems width =<< yanksFor operators
    void (staticNvimMenuWith (fuzzyItemFilter False) def (toList items) consumer promptConfig)
  where
    consumer =
      Consumer.withMappings yankMenuMappings

uraYankMenu ::
  Members (YankMenuStack res) r =>
  Handler r ()
uraYankMenu =
  uraYankMenuFor Nothing
