module Uracil.Test.YankMenuTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Control.Lens ((.~))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Polysemy.Test (TestError, UnitTest, unitTest, (===), assertLeft)
import Ribosome (Rpc, Scratch)
import Ribosome.Api (currentBufferContent, getreg, setCurrentBufferContent, setCurrentLine, unnamedRegister, syntheticInput)
import qualified Ribosome.Register as Register
import Ribosome.Test (testHandler, testError)
import Test.Tasty (TestTree, testGroup)

import Uracil.Data.Yank (Yank (Yank))
import Uracil.Test.Run (uraTest)
import Uracil.YankMenu (YankMenuStack, yankMenuWith)
import Ribosome.Menu (PromptListening, defaultPrompt, interpretMenu)
import Time (MilliSeconds, convert)
import qualified Sync
import Conc (withAsync_)
import Uracil.Data.YankError (YankError)

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
  Members YankMenuStack r =>
  Members [Rpc, Scratch, Error TestError, Async] r =>
  [Text] ->
  Sem r ()
yankMenuTest chars =
  testError @YankError do
    atomicModify' (#yanks .~ items)
    setCurrentBufferContent ["1", "2", "3"]
    setCurrentLine 1
    promptConfig <- defaultPrompt []
    interpretMenu do
      withPromptInput (Just 50) chars (testHandler (yankMenuWith promptConfig Nothing))

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
