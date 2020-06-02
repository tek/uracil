{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PasteSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Prelude hiding (integrationSpecDef)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Register (getreg, setregLine, starRegister, unnamedRegister)
import Ribosome.Api.Window (setCurrentCursor, setCurrentLine)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.IO (vimGetWindows, vimSetOption)
import Test.Framework

import qualified Ribosome.Data.Register as Register (Register(Special))
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType(Line))
import Unit
import Uracil.Data.Env (Env, Uracil)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.RegEvent (RegEvent(RegEvent))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Paste (uraPaste, uraPasteFor, uraPpaste)
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Yank (storeEvent)

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
  [
    item (Ident.Str "1") "y" item1,
    item (Ident.Str "2") "d" item2,
    item (Ident.Str "3") "y" item3
  ]
  where
    item ident =
      Yank ident (Register.Special "*") RegisterType.Line

clearStar :: Uracil ()
clearStar =
  setregLine starRegister [""]

normalPasteSpec :: Uracil ()
normalPasteSpec = do
  clearStar
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
  tmuxSpecDef normalPasteSpec

normalPpasteSpec :: Uracil ()
normalPpasteSpec = do
  clearStar
  setL @Env Env.yanks yanks
  uraPpaste
  checkContent item1
  gassertEqual 2 . length =<< vimGetWindows
  uraPpaste
  checkContent item2
  await (gassertEqual 1 . length) vimGetWindows
  where
    checkContent item =
      await (gassertEqual (NonEmpty.toList item ++ [""])) currentBufferContent

test_normalPpaste :: IO ()
test_normalPpaste =
  tmuxSpecDef normalPpasteSpec

visualPasteSpec :: Uracil ()
visualPasteSpec = do
  clearStar
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
  gassertEqual (Left (NonEmpty.toList item1)) =<< getreg unnamedRegister
  where
    checkContent item =
      await (gassertEqual ("line1" : "word " : NonEmpty.toList item ++ [" word"])) currentBufferContent

test_visualPaste :: IO ()
test_visualPaste =
  tmuxSpecDef visualPasteSpec

cancelWhenCursorMovedSpec :: Uracil ()
cancelWhenCursorMovedSpec = do
  updateSetting Settings.pasteTimeout 93
  setL @Env Env.yanks yanks
  uraPaste
  gassertEqual 2 . length =<< vimGetWindows
  doautocmd False "CursorMoved"
  await (gassertEqual 1 . length) vimGetWindows

test_cancelWhenCursorMoved :: IO ()
test_cancelWhenCursorMoved =
  integrationSpecDef cancelWhenCursorMovedSpec

syncSelectionSpec :: Uracil ()
syncSelectionSpec = do
  setL @Env Env.yanks yanks
  setCurrentBufferContent ["line1", "line2"]
  setCurrentLine 0
  setregLine starRegister [extra]
  uraPaste
  gassertEqual ["line1", extra, "line2"] =<< currentBufferContent
  where
    extra =
      "external" :: Text

test_syncSelection :: IO ()
test_syncSelection =
  tmuxSpecDef syncSelectionSpec

operatorPasteSpec :: Uracil ()
operatorPasteSpec = do
  clearStar
  storeEvent (RegEvent True "y" ["line 1"] unnamedRegister RegisterType.Line)
  storeEvent (RegEvent True "d" ["line 2"] unnamedRegister RegisterType.Line)
  setregLine unnamedRegister ["line 2"]
  uraPasteFor (Just "y")
  checkContent ["line 1"]
  where
    checkContent ls =
      await (gassertEqual ("" : ls)) currentBufferContent

test_operatorPaste :: IO ()
test_operatorPaste =
  tmuxSpecDef operatorPasteSpec
