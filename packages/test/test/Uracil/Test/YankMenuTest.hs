module Uracil.Test.YankMenuTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Conc (interpretAtomic, interpretMaskFinal)
import Control.Lens ((.~))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Polysemy.Test (TestError, UnitTest, unitTest, (===))
import Ribosome (Rpc)
import Ribosome.Api (currentBufferContent, getreg, setCurrentBufferContent, setCurrentLine, unnamedRegister, withInput)
import qualified Ribosome.Register as Register
import Ribosome.Test (embedPluginTest_, resumeTestError, testHandler)
import Test.Tasty (TestTree, testGroup)

import Uracil.Data.Yank (Yank (Yank))
import Uracil.YankMenu (YankMenuStack, uraYankMenu)

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
  Members (YankMenuStack res) r =>
  Members [Error TestError, Async] r =>
  [Text] ->
  Sem r ()
yankMenuTest chars =
  resumeTestError @Rpc do
    atomicModify' (#yanks .~ items)
    setCurrentBufferContent ["1", "2", "3"]
    setCurrentLine 1
    withInput Nothing (Just 50) chars (testHandler uraYankMenu)

yankChars :: [Text]
yankChars =
  ["k", "k", "k", "y"]

test_yankMenuYank :: UnitTest
test_yankMenuYank =
  embedPluginTest_ $ interpretMaskFinal $ interpretAtomic def do
    yankMenuTest yankChars
    (Left (NonEmpty.toList targetItem) ===) =<< getreg unnamedRegister

pasteChars :: [Text]
pasteChars =
  ["k", "k", "k", "p"]

test_yankMenuPaste :: UnitTest
test_yankMenuPaste =
  embedPluginTest_ $ interpretMaskFinal $ interpretAtomic def do
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
  embedPluginTest_ $ interpretMaskFinal $ interpretAtomic def do
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
