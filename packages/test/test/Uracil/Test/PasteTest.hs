module Uracil.Test.PasteTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Hedgehog ((===))
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Register (getreg, setregLine, starRegister, unnamedRegister)
import Ribosome.Api.Window (setCurrentCursor, setCurrentLine)
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Data.Register as Register (Register(Special))
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType(Line))
import Ribosome.Nvim.Api.IO (vimGetWindows, vimSetOption)
import Ribosome.Test.Await (await, awaitEqual, awaitEqual_)
import Ribosome.Test.Run (UnitTest, unitTest)
import Test.Tasty (TestTree, testGroup)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.RegEvent (RegEvent(RegEvent))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Paste (uraPaste, uraPasteFor, uraPpaste)
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Test.Unit (UracilTest, integrationTestDef, tmuxTestDef)
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

clearStar :: UracilTest ()
clearStar =
  setregLine starRegister [""]

normalPasteTest :: UracilTest ()
normalPasteTest = do
  clearStar
  vimSetOption "clipboard" (toMsgpack ("unnamed,unnamedplus" :: Text))
  setL @Env Env.yanks yanks
  uraPaste
  checkContent item1
  (2 ===) . length =<< vimGetWindows
  uraPaste
  checkContent item2
  awaitEqual 1 length vimGetWindows
  where
    checkContent item =
      awaitEqual_ ("" : NonEmpty.toList item) currentBufferContent

test_normalPaste :: UnitTest
test_normalPaste =
  tmuxTestDef normalPasteTest

normalPpasteTest :: UracilTest ()
normalPpasteTest = do
  clearStar
  setL @Env Env.yanks yanks
  uraPpaste
  checkContent item1
  (2 ===) . length =<< vimGetWindows
  uraPpaste
  checkContent item2
  await ((1 ===) . length) vimGetWindows
  where
    checkContent item =
      await ((NonEmpty.toList item ++ [""]) ===) currentBufferContent

test_normalPpaste :: UnitTest
test_normalPpaste =
  tmuxTestDef normalPpasteTest

visualPasteTest :: UracilTest ()
visualPasteTest = do
  clearStar
  setL @Env Env.yanks yanks
  setCurrentBufferContent ["line1", "word for word", ""]
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
  (Left (NonEmpty.toList item1) ===) =<< getreg unnamedRegister
  where
    checkContent item =
      awaitEqual_ ("line1" : "word " : NonEmpty.toList item ++ [" word", ""]) currentBufferContent

test_visualPaste :: UnitTest
test_visualPaste =
  tmuxTestDef visualPasteTest

cancelWhenCursorMovedTest :: UracilTest ()
cancelWhenCursorMovedTest = do
  updateSetting Settings.pasteTimeout 93
  setL @Env Env.yanks yanks
  uraPaste
  (2 ===) . length =<< vimGetWindows
  doautocmd False "CursorMoved"
  awaitEqual 1 length vimGetWindows

test_cancelWhenCursorMoved :: UnitTest
test_cancelWhenCursorMoved =
  integrationTestDef cancelWhenCursorMovedTest

syncSelectionTest :: UracilTest ()
syncSelectionTest = do
  setL @Env Env.yanks yanks
  setCurrentBufferContent ["line1", "line2"]
  setCurrentLine 0
  setregLine starRegister [extra]
  uraPaste
  (["line1", extra, "line2"] ===) =<< currentBufferContent
  where
    extra =
      "external" :: Text

test_syncSelection :: UnitTest
test_syncSelection =
  tmuxTestDef syncSelectionTest

commandPasteTest :: UracilTest ()
commandPasteTest = do
  clearStar
  storeEvent (RegEvent True "y" ["line 1"] unnamedRegister RegisterType.Line)
  storeEvent (RegEvent True "d" ["line 2"] unnamedRegister RegisterType.Line)
  setregLine unnamedRegister ["line 2"]
  uraPasteFor (Just "y")
  checkContent ["line 1"]
  where
    checkContent ls =
      awaitEqual_ ("" : ls) currentBufferContent

test_commandPaste :: UnitTest
test_commandPaste =
  tmuxTestDef commandPasteTest

test_paste :: TestTree
test_paste =
  testGroup "paste" [
    unitTest "normal mode, paste after" test_normalPaste,
    unitTest "normal mode, paste before" test_normalPpaste,
    unitTest "visual mode" test_visualPaste,
    unitTest "cancel when cursor moved" test_cancelWhenCursorMoved,
    unitTest "sync the * register" test_syncSelection,
    unitTest "paste from a yank list for a specific command" test_commandPaste
  ]
