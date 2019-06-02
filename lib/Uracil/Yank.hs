module Uracil.Yank where

import Chiasma.Data.Ident (Ident, generateIdent, sameIdent)
import Conduit (yieldMany)
import qualified Control.Lens as Lens (filtered, firstOf, folded)
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
import Ribosome.Menu.Simple (defaultMenu, menuQuit, menuQuitWith, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimGetVvar)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.RegEvent (RegEvent(RegEvent))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Data.YankError (YankError)
import qualified Uracil.Data.YankError as YankError (YankError(Empty, NoSuchYank))

storeEvent ::
  MonadRibo m =>
  RegEvent ->
  MonadDeepState s Env m =>
  m ()
storeEvent (RegEvent _ "y" content register regtype) = do
  ident <- generateIdent
  let yank = Yank ident register regtype content
  showDebug "yank" yank
  prependUnique @Env Env.yanks yank

uraYank ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  m ()
uraYank =
  catchAs @DecodeError () (storeEvent =<< vimGetVvar "event")

yanks ::
  MonadDeepState s Env m =>
  m [Yank]
yanks =
  getL @Env Env.yanks

nonEmptyYanks ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  m (NonEmpty Yank)
nonEmptyYanks =
  hoistMaybe YankError.Empty . nonEmpty =<< yanks

yankByIdent ::
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m Yank
yankByIdent ident =
  hoistMaybe (YankError.NoSuchYank ident) =<< gets @Env (Lens.firstOf lens)
  where
    lens =
      Env.yanks . Lens.folded . Lens.filtered (sameIdent ident)

loadYank ::
  NvimE e m =>
  Text ->
  Yank ->
  m ()
loadYank register (Yank _ _ tpe text) =
  vimCallFunction "setreg" [toMsgpack register, toMsgpack text, toMsgpack tpe]

loadYankIdent ::
  NvimE e m =>
  MonadDeepState s Env m =>
  MonadDeepError e YankError m =>
  Ident ->
  m ()
loadYankIdent =
  loadYank "\"" <=< yankByIdent
