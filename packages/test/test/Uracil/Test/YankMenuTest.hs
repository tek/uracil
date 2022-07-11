module Uracil.Test.YankMenuTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Conc (ConcStack, withAsync_)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (TestError, UnitTest, assertLeft, unitTest, (===))
import Ribosome (Rpc, RpcError, Scratch, SettingError, Settings)
import Ribosome.Api (currentBufferContent, getreg, setCurrentBufferContent, setCurrentLine, syntheticInput, unnamedRegister)
import Ribosome.Menu (PromptListening, interpretNvimMenuFinal, promptInput)
import qualified Ribosome.Register as Register
import Ribosome.Test (testError, testHandler)
import qualified Sync
import Test.Tasty (TestTree, testGroup)
import Time (MilliSeconds, convert)

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

withPromptInput ::
  Members [Sync PromptListening, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInput interval chrs =
  withAsync_ (Sync.takeBlock *> syntheticInput (convert <$> interval) chrs)

yankMenuTest ::
  Members ConcStack r =>
  Members UracilStack r =>
  Members [ChronosTime, Log] r =>
  Members [Rpc, Rpc !! RpcError, Scratch !! RpcError, Settings !! SettingError, Error TestError] r =>
  [Text] ->
  Sem r ()
yankMenuTest chars =
  testError @YankError do
    atomicModify' (#yanks .~ items)
    setCurrentBufferContent ["1", "2", "3"]
    setCurrentLine 1
    interpretNvimMenuFinal $ promptInput chars do
      testHandler (yankMenuWith Nothing)

yankChars :: [Text]
yankChars =
  ["k", "k", "k", "y"]

test_yankMenuYank :: UnitTest
test_yankMenuYank =
  uraTest do
    yankMenuTest yankChars
    assertLeft (NonEmpty.toList targetItem) =<< getreg unnamedRegister

pasteChars :: [Text]
pasteChars =
  ["k", "k", "k", "p"]

test_yankMenuPaste :: UnitTest
test_yankMenuPaste =
  uraTest do
    yankMenuTest pasteChars
    (target ===) =<< currentBufferContent
  where
    target =
      ["1", "2"] <> NonEmpty.toList targetItem <> ["3"]

ppasteChars :: [Text]
ppasteChars =
  ["k", "k", "k", "P"]

test_yankMenuPpaste :: UnitTest
test_yankMenuPpaste =
  uraTest do
    yankMenuTest ppasteChars
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
