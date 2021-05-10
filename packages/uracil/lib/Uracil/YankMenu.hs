module Uracil.YankMenu where

import Chiasma.Data.Ident (Ident)
import Control.Lens (view)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Map.Strict as Map (fromList)
import qualified Data.Text as Text (length, take)
import Ribosome.Menu.Action (menuQuitWith)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (strictNvimMenu)
import Ribosome.Menu.Simple (MappingHandler, Mappings, defaultMenu, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetCurrentWindow, windowGetWidth)

import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Data.YankCommand (YankCommand)
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(EmptyHistory, InvalidMenuIndex))
import Uracil.Paste (pasteIdent, ppasteIdent)
import Uracil.Yank (loadYankIdent, yanksFor)

menuAction ::
  NvimE e m =>
  MonadDeepError e YankError m =>
  (Ident -> m ()) ->
  Menu Ident ->
  Prompt ->
  m (MenuConsumerAction m (), Menu Ident)
menuAction action m _ = do
  ident <- view MenuItem.meta <$> hoistMaybe YankError.InvalidMenuIndex item
  menuQuitWith (action ident) m
  where
    item =
      selectedMenuItem m

menuYank ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MappingHandler m () Ident
menuYank =
  menuAction loadYankIdent

menuPaste ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MappingHandler m () Ident
menuPaste =
  menuAction pasteIdent

menuPpaste ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MappingHandler m () Ident
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
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Mappings m () Ident
yankMenuMappings =
  Map.fromList [("p", menuPaste), ("P", menuPpaste), ("y", menuYank)]

uraYankMenuFor ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MonadDeepError e DecodeError m =>
  Maybe YankCommand ->
  m ()
uraYankMenuFor operators = do
  width <- windowGetWidth =<< vimGetCurrentWindow
  void $ run =<< yankMenuItems width <$> yanksFor operators
  where
    run [] =
      throwHoist YankError.EmptyHistory
    run items =
      strictNvimMenu def items handler promptConfig Nothing
    handler =
      defaultMenu yankMenuMappings
    promptConfig =
      PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer []

uraYankMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MonadDeepError e DecodeError m =>
  m ()
uraYankMenu =
  uraYankMenuFor Nothing
