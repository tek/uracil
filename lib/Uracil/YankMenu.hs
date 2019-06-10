module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident, identText, parseIdent)
import Conduit (yieldMany)
import qualified Control.Lens as Lens (view)
import qualified Data.Map.Strict as Map (fromList)
import qualified Data.Text as Text (length, take)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (ident)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (MappingHandler, Mappings, defaultMenu, menuQuitWith, menuQuitWith, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetCurrentWindow, windowGetHeight)

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory, InvalidMenuIndex))
import Uracil.Paste (pasteIdent, ppasteIdent)
import Uracil.Yank (loadYankIdent, yanks)

menuAction ::
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  (Ident -> m ()) ->
  Menu ->
  Prompt ->
  m (MenuConsumerAction m (), Menu)
menuAction action m _ = do
  ident <- Lens.view MenuItem.ident <$> hoistMaybe YankError.InvalidMenuIndex item
  menuQuitWith (action (parseIdent ident)) m
  where
    item =
      selectedMenuItem m

menuYank ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MappingHandler m ()
menuYank =
  menuAction loadYankIdent

menuPaste ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MappingHandler m ()
menuPaste =
  menuAction pasteIdent

menuPpaste ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MappingHandler m ()
menuPpaste =
  menuAction ppasteIdent

yankMenuItems :: Int -> [Yank] -> [MenuItem]
yankMenuItems width yanks' =
  uncurry menuItem <$> zip yanks' [(0 :: Int)..]
  where
    menuItem (Yank ident _ _ (line' :| rest)) _ =
      MenuItem (identText ident) (Text.take maxlen line' <> dots line' <> count rest)
    dots line' =
      if Text.length line' > maxlen then "..." else ""
    count [] =
      ""
    count ls =
      " [" <> show (length ls + 1) <> "]"
    maxlen =
      width - 6

yankMenuMappings ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Mappings m ()
yankMenuMappings =
  Map.fromList [("p", menuPaste), ("P", menuPpaste), ("y", menuYank)]

uraYankMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MonadDeepError e DecodeError m =>
  m ()
uraYankMenu = do
  width <- windowGetHeight =<< vimGetCurrentWindow
  void $ run =<< yankMenuItems width <$> yanks
  where
    run [] =
      throwHoist YankError.EmptyHistory
    run items =
      nvimMenu def (yieldMany items) handler promptConfig
    handler =
      defaultMenu yankMenuMappings
    promptConfig =
      PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer False
