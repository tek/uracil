module Uracil.Clipboard where

import Chiasma.Data.Ident (Ident)
import Data.List (notElem)
import Ribosome (Rpc)
import Ribosome.Api (getregLines, getregtype)
import Ribosome.Register (Register)
import qualified Ribosome.Register as Register (Register (..))

import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import qualified Uracil.Data.Yank as Yank (content)
import Uracil.Yank (allYanks, storeYank)

pullRegister ::
  Members [Rpc, AtomicState Env, Log, Input Ident] r =>
  Register ->
  NonEmpty Text ->
  Sem r ()
pullRegister register content = do
  tpe <- getregtype register
  storeYank tpe register "y" content

fetchClipboard ::
  Members [Rpc, AtomicState Env, Log, Input Ident] r =>
  [NonEmpty Text] ->
  Maybe (NonEmpty Text) ->
  Register ->
  Sem r ()
fetchClipboard lastTwoYanks skip reg =
  traverse_ check . nonEmpty =<< getregLines reg
  where
    check content =
      when (freshYank content) (pullRegister reg content)
    freshYank a =
      a /= ("" :| []) && notElem a lastTwoYanks && notElem a skip

syncClipboard ::
  Members [Rpc, AtomicState Env, Log, Input Ident] r =>
  Sem r ()
syncClipboard = do
  lastTwoYanks <- take 2 . fmap (.content) <$> allYanks
  skip <- atomicGets (.skip)
  atomicModify' (#skip .~ Nothing)
  traverse_ @[] (fetchClipboard lastTwoYanks skip) [Register.Special "*", Register.Special "\""]
