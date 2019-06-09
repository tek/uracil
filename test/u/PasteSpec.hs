{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PasteSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Window (setCurrentCursor)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.IO (vimGetWindows, vimSetOption)
import Test.Framework

import Unit (integrationSpecDef, tmuxGuiSpecDef)
import Uracil.Data.Env (Env, Uracil)
import qualified Uracil.Data.Env as Env (yanks)
import qualified Uracil.Data.Register as Register (Register(Special))
import qualified Uracil.Data.RegisterType as RegisterType (RegisterType(Line))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Paste (uraPaste)
import qualified Uracil.Settings as Settings (pasteTimeout)

item1 :: NonEmpty Text
item1 =
  "item1" :| []

item2 :: NonEmpty Text
item2 =
  "item2" :| ["item2 cont"]

item3 :: NonEmpty Text
item3 =
  "item3" :| []

yanks :: [Yank]
yanks =
  uncurry item <$> [
    (Ident.Str "1", item1),
    (Ident.Str "2", item2),
    (Ident.Str "3", item3)
    ]
  where
    item ident =
      Yank ident (Register.Special "*") RegisterType.Line

normalPasteSpec :: Uracil ()
normalPasteSpec = do
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

test_normalPaste :: IO ()
test_normalPaste =
  tmuxGuiSpecDef normalPasteSpec

visualPasteSpec :: Uracil ()
visualPasteSpec = do
  setL @Env Env.yanks yanks
  setCurrentBufferContent ["line1", "word for word"]
  setCurrentCursor 1 5
  normal "viw"
  uraPaste
  checkContent item1
  uraPaste
  checkContent item2
  uraPaste
  checkContent item3
  uraPaste
  checkContent item1
  where
    checkContent item =
      await (gassertEqual ("line1" : "word " : NonEmpty.toList item ++ [" word"])) currentBufferContent

test_visualPaste :: IO ()
test_visualPaste =
  tmuxGuiSpecDef visualPasteSpec

cancelWhenCursorMovedSpec :: Uracil ()
cancelWhenCursorMovedSpec = do
  updateSetting Settings.pasteTimeout 93
  setL @Env Env.yanks yanks
  uraPaste
  gassertEqual 2 . length =<< vimGetWindows
  doautocmd "CursorMoved"
  await (gassertEqual 1 . length) vimGetWindows

test_cancelWhenCursorMoved :: IO ()
test_cancelWhenCursorMoved =
  integrationSpecDef cancelWhenCursorMovedSpec
