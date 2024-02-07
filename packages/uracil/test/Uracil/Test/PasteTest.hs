module Uracil.Test.PasteTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Polysemy.Test (UnitTest, assertEq, unitTest, (===))
import Ribosome (Rpc, Settings, mapReport)
import Ribosome.Api (
  currentBufferContent,
  getreg,
  normal,
  nvimInput,
  nvimSetKeymap,
  setCurrentBufferContent,
  setCurrentCursor,
  setCurrentLine,
  setregLine,
  starRegister,
  unnamedRegister,
  vimGetWindows,
  )
import qualified Ribosome.Register as Register (Register (Special), RegisterType (Line))
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, resumeTestError, testPluginConf)
import Ribosome.Test.Wait ((<--))
import Test.Tasty (TestTree, testGroup)

import qualified Uracil.Data.Env as Env
import Uracil.Data.RegEvent (RegEvent (RegEvent))
import Uracil.Data.Yank (Yank (Yank))
import Uracil.Paste (uraPaste, uraPasteFor, uraPpaste, uraStopPaste)
import Uracil.Plugin (UracilProdStack, handlers, interpretUracilProdStack)
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Test.Run (testConfig, uraTest)
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
      Yank ident (Register.Special "*") Register.Line

clearStar ::
  Member Rpc r =>
  Sem r ()
clearStar =
  setregLine starRegister [""]

test_normalPaste :: UnitTest
test_normalPaste =
  uraTest do
    clearStar
    atomicModify' (#yanks .~ yanks)
    uraPaste
    checkContent item1
    assertEq 2 . length =<< vimGetWindows
    uraPaste
    checkContent item2
    assertWait vimGetWindows (assertEq 1 . length)
  where
    checkContent item =
      assertWait currentBufferContent (assertEq ("" : NonEmpty.toList item))

test_normalPpaste :: UnitTest
test_normalPpaste =
  uraTest do
    clearStar
    atomicModify' (#yanks .~ yanks)
    setCurrentBufferContent ["line1", "line2", ""]
    setCurrentCursor 1 0
    uraPpaste
    checkContent item1
    assertEq 2 . length =<< vimGetWindows
    uraPpaste
    checkContent item2
    assertWait vimGetWindows (assertEq 1 . length)
  where
    checkContent item =
      assertWait currentBufferContent (assertEq ("line1" : NonEmpty.toList item ++ ["line2", ""]))

test_visualPaste :: UnitTest
test_visualPaste =
  uraTest do
    clearStar
    atomicModify' (#yanks .~ yanks)
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
      assertWait currentBufferContent (assertEq ("line1" : "word " : NonEmpty.toList item ++ [" word", ""]))

test_cancel :: UnitTest
test_cancel =
  uraTest do
    resumeTestError @Settings (Settings.update Settings.pasteTimeout 93)
    atomicModify' (#yanks .~ yanks)
    uraPaste
    (2 ===) . length =<< vimGetWindows
    uraStopPaste
    assertWait vimGetWindows (assertEq 1 . length)

test_syncSelection :: UnitTest
test_syncSelection =
  uraTest do
    atomicModify' (#yanks .~ yanks)
    setCurrentBufferContent ["line1", "line2"]
    setCurrentLine 0
    setregLine starRegister [extra]
    uraPaste
    (["line1", extra, "line2"] ===) =<< currentBufferContent
  where
    extra =
      "external" :: Text

test_commandPaste :: UnitTest
test_commandPaste =
  uraTest do
    clearStar
    mapReport do
      storeEvent (RegEvent True "y" ["line 1"] unnamedRegister Register.Line)
      storeEvent (RegEvent True "d" ["line 2"] unnamedRegister Register.Line)
    setregLine unnamedRegister ["line 2"]
    uraPasteFor (Just "y")
    checkContent ["line 1"]
  where
    checkContent ls =
      assertWait currentBufferContent (assertEq ("" : ls))

test_commandRegister :: UnitTest
test_commandRegister = do
  testPluginConf @UracilProdStack (testConfig def) interpretUracilProdStack handlers do
    nvimSetKeymap "n" "p" "<cmd>UraPaste<cr>" mempty
    setCurrentBufferContent ["1", "2", "3"]
    assertEq 0 . length =<< atomicGets (.yanks)
    void (nvimInput "\"fyy")
    setregLine unnamedRegister ["4"]
    void (nvimInput "\"fp")
    ["1", "1", "2", "3"] <-- currentBufferContent
    void (nvimInput "p")
    ["1", "1", "4", "2", "3"] <-- currentBufferContent

test_paste :: TestTree
test_paste =
  testGroup "paste" [
    unitTest "normal mode, paste after" test_normalPaste,
    unitTest "normal mode, paste before" test_normalPpaste,
    unitTest "visual mode" test_visualPaste,
    unitTest "cancel" test_cancel,
    unitTest "sync the * register" test_syncSelection,
    unitTest "paste from a yank list for a specific command" test_commandPaste,
    unitTest "use register from command/mapping prefix" test_commandRegister
  ]
