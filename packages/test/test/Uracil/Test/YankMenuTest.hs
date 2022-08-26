module Uracil.Test.YankMenuTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Conc (ChanConsumer, ConcStack)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (TestError, UnitTest, assertLeft, unitTest, (===))
import Ribosome (Event, Rpc, RpcError, Scratch, SettingError, Settings)
import Ribosome.Api (currentBufferContent, getreg, setCurrentBufferContent, setCurrentLine, unnamedRegister)
import Ribosome.Menu (promptInput)
import Ribosome.Menu.Prompt (PromptEvent (Mapping))
import qualified Ribosome.Register as Register
import Ribosome.Test (testError, testHandler)
import Test.Tasty (TestTree, testGroup)

import Uracil.Data.Yank (Yank (Yank))
import Uracil.Data.YankError (YankError)
import Uracil.Plugin (UracilStack)
import Uracil.Test.Run (uraTest)
import Uracil.YankMenu (yankMenuWith)

targetItem :: NonEmpty Text
targetItem =
  "item4" :| ["item4 cont"]

items :: [Yank]
items =
  uncurry item <$> [
    (Ident.Str "1", "item1" :| []),
    (Ident.Str "2", "item2" :| ["item2 cont"]),
    (Ident.Str "3", "item3" :| []),
    (Ident.Str "4", targetItem),
    (Ident.Str "5", "item5" :| [])
    ]
  where
    item ident =
      Yank ident (Register.Special "*") Register.Line "y"

yankMenuTest ::
  Members ConcStack r =>
  Members UracilStack r =>
  Members [ChanConsumer Event, ChronosTime, Log] r =>
  Members [Rpc, Rpc !! RpcError, Scratch !! RpcError, Settings !! SettingError, Error TestError] r =>
  [PromptEvent] ->
  Sem r ()
yankMenuTest events =
  testError @YankError do
    atomicModify' (#yanks .~ items)
    setCurrentBufferContent ["1", "2", "3"]
    setCurrentLine 1
    promptInput events do
      testHandler (yankMenuWith Nothing)

yankEvents :: [PromptEvent]
yankEvents =
  Mapping <$> ["k", "k", "k", "y"]

test_yankMenuYank :: UnitTest
test_yankMenuYank =
  uraTest do
    yankMenuTest yankEvents
    assertLeft (NonEmpty.toList targetItem) =<< getreg unnamedRegister

pasteEvents :: [PromptEvent]
pasteEvents =
  Mapping <$> ["k", "k", "k", "p"]

test_yankMenuPaste :: UnitTest
test_yankMenuPaste =
  uraTest do
    yankMenuTest pasteEvents
    (target ===) =<< currentBufferContent
  where
    target =
      ["1", "2"] <> NonEmpty.toList targetItem <> ["3"]

ppasteEvents :: [PromptEvent]
ppasteEvents =
  Mapping <$> ["k", "k", "k", "P"]

test_yankMenuPpaste :: UnitTest
test_yankMenuPpaste =
  uraTest do
    yankMenuTest ppasteEvents
    (target ===) =<< currentBufferContent
  where
    target =
      ["1"] <> NonEmpty.toList targetItem <> ["2", "3"]

test_yankMenu :: TestTree
test_yankMenu =
  testGroup "yank menu" [
    unitTest "yank from menu" test_yankMenuYank,
    unitTest "paste from menu" test_yankMenuPaste,
    unitTest "paste before from menu" test_yankMenuPpaste
  ]
