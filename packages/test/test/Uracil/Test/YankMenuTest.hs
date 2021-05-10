module Uracil.Test.YankMenuTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Hedgehog ((===))
import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Register (getreg, unnamedRegister)
import Ribosome.Api.Window (setCurrentLine)
import qualified Ribosome.Data.Register as Register (Register(Special))
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType(Line))
import Ribosome.Test.Input (withInput)
import Ribosome.Test.Run (UnitTest, unitTest)
import Test.Tasty (TestTree, testGroup)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Test.Unit (UracilTest, tmuxTestDef)
import Uracil.YankMenu (uraYankMenu)

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
      Yank ident (Register.Special "*") RegisterType.Line "y"

yankMenuTest :: [Text] -> UracilTest ()
yankMenuTest chars = do
  setL @Env Env.yanks items
  setCurrentBufferContent ["1", "2", "3"]
  setCurrentLine 1
  withInput (Just 0.05) chars uraYankMenu

yankChars :: [Text]
yankChars =
  ["k", "k", "k", "y"]

yankMenuYankTest :: UracilTest ()
yankMenuYankTest = do
  yankMenuTest yankChars
  (Left (NonEmpty.toList targetItem) ===) =<< getreg unnamedRegister

test_yankMenuYank :: UnitTest
test_yankMenuYank =
  tmuxTestDef yankMenuYankTest

pasteChars :: [Text]
pasteChars =
  ["k", "k", "k", "p"]

yankMenuPasteTest :: UracilTest ()
yankMenuPasteTest = do
  yankMenuTest pasteChars
  (target ===) =<< currentBufferContent
  where
    target =
      ["1", "2"] <> NonEmpty.toList targetItem <> ["3"]

test_yankMenuPaste :: UnitTest
test_yankMenuPaste =
  tmuxTestDef yankMenuPasteTest

ppasteChars :: [Text]
ppasteChars =
  ["k", "k", "k", "P"]

yankMenuPpasteTest :: UracilTest ()
yankMenuPpasteTest = do
  yankMenuTest ppasteChars
  (target ===) =<< currentBufferContent
  where
    target =
      ["1"] <> NonEmpty.toList targetItem <> ["2", "3"]

test_yankMenuPpaste :: UnitTest
test_yankMenuPpaste =
  tmuxTestDef yankMenuPpasteTest

test_yankMenu :: TestTree
test_yankMenu =
  testGroup "yank menu" [
    unitTest "yank from menu" test_yankMenuYank,
    unitTest "paste from menu" test_yankMenuPaste,
    unitTest "paste before from menu" test_yankMenuPpaste
  ]
