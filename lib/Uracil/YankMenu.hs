module Uracil.YankMenu where

import Chiasma.Data.Ident (identText, parseIdent)
import Conduit (yieldMany)
import qualified Control.Lens as Lens (view)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (length, take, unwords)
import Ribosome.Control.Monad.Ribo (prependUnique)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (ident)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, menuQuitWith, menuQuitWith, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetVvar)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.RegEvent (RegEvent(RegEvent))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(Empty, InvalidMenuIndex))
import Uracil.Paste (pasteIdent)
import Uracil.Yank (yanks)

menuPaste ::
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Menu ->
  Prompt ->
  m (MenuConsumerAction m (), Menu)
menuPaste m _ = do
  ident <- Lens.view MenuItem.ident <$> hoistMaybe YankError.InvalidMenuIndex item
  menuQuitWith (pasteIdent (parseIdent ident)) m
  where
    item =
      selectedMenuItem m

yankMenuItems :: [Yank] -> [MenuItem]
yankMenuItems yanks =
  uncurry menuItem <$> zip yanks [0..]
  where
    menuItem (Yank ident _ _ (line' : rest)) index =
      MenuItem (identText ident) (Text.take 30 line' <> dots line' <> count rest)
    dots line' =
      if Text.length line' > 30 then "..." else ""
    count [] =
      ""
    count ls =
      " [" <> show (length ls + 1) <> "]"

uraYankMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  MonadDeepError e DecodeError m =>
  m ()
uraYankMenu =
  void $ run =<< yankMenuItems <$> yanks
  where
    run [] =
      throwHoist YankError.Empty
    run items =
      nvimMenu def (yieldMany items) handler promptConfig
    handler =
      defaultMenu (Map.fromList [("p", menuPaste)])
    promptConfig =
      PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer False
