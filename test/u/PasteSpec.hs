{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PasteSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Nvim.Api.IO (vimGetWindows, vimSetOption)
import Test.Framework
import Unit (tmuxGuiSpecDef)

import Uracil.Data.Env (Env, Uracil)
import qualified Uracil.Data.Env as Env (yanks)
import qualified Uracil.Data.Register as Register (Register(Special))
import qualified Uracil.Data.RegisterType as RegisterType (RegisterType(Line))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Paste (uraPaste)

item1 :: NonEmpty Text
item1 =
  "item1" :| []

item2 :: NonEmpty Text
item2 =
  "item2" :| ["item2 cont"]

yanks :: [Yank]
yanks =
  uncurry item <$> [
    (Ident.Str "1", item1),
    (Ident.Str "2", item2),
    (Ident.Str "3", "item3" :| [])
    ]
  where
    item ident =
      Yank ident (Register.Special "*") RegisterType.Line

pasteCycleSpec :: Uracil ()
pasteCycleSpec = do
  vimSetOption "clipboard" (toMsgpack ("unnamed,unnamedplus" :: Text))
  setL @Env Env.yanks yanks
  uraPaste
  checkContent item1
  gassertEqual 2 . length =<< vimGetWindows
  uraPaste
  checkContent item2
  await (gassertEqual 1 . length) vimGetWindows
  where
    checkContent item =
      await (gassertEqual ("" : NonEmpty.toList item)) currentBufferContent

test_pasteCycle :: IO ()
test_pasteCycle =
  tmuxGuiSpecDef pasteCycleSpec
