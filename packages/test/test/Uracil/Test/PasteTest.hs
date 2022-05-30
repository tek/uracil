module Uracil.Test.PasteTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Control.Lens ((.~))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Polysemy.Test (UnitTest, assertEq, unitTest, (===))
import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Register (getreg, setregLine, starRegister, unnamedRegister)
import Ribosome.Api.Window (setCurrentCursor, setCurrentLine)
import qualified Ribosome.Data.Register as Register (Register (Special))
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType (Line))
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host (Rpc)
import Ribosome.Host.Api.Effect (vimGetWindows)
import Ribosome.Host.Data.HandlerError (mapHandlerError)
import Ribosome.Test.Error (resumeTestError)
import Ribosome.Test.Wait (assertWait)
import Test.Tasty (TestTree, testGroup)

import Uracil.Data.RegEvent (RegEvent (RegEvent))
import Uracil.Data.Yank (Yank (Yank))
import Uracil.Paste (uraPaste, uraPasteFor, uraPpaste, uraStopPaste)
import qualified Uracil.Settings as Settings (pasteTimeout)
import Uracil.Test.Run (uraTest)
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
    mapHandlerError do
      storeEvent (RegEvent True "y" ["line 1"] unnamedRegister RegisterType.Line)
      storeEvent (RegEvent True "d" ["line 2"] unnamedRegister RegisterType.Line)
    setregLine unnamedRegister ["line 2"]
    uraPasteFor (Just "y")
    checkContent ["line 1"]
  where
    checkContent ls =
      assertWait currentBufferContent (assertEq ("" : ls))

test_paste :: TestTree
test_paste =
  testGroup "paste" [
    unitTest "normal mode, paste after" test_normalPaste,
    unitTest "normal mode, paste before" test_normalPpaste,
    unitTest "visual mode" test_visualPaste,
    unitTest "cancel" test_cancel,
    unitTest "sync the * register" test_syncSelection,
    unitTest "paste from a yank list for a specific command" test_commandPaste
  ]
