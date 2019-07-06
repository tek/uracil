{-# OPTIONS_GHC -F -pgmF htfpp #-}

module YankMenuSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Register (getreg, unnamedRegister)
import Ribosome.Api.Window (setCurrentLine)
import Ribosome.Test.Input (withInput)
import Test.Framework

import qualified Ribosome.Data.Register as Register (Register(Special))
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType(Line))
import Unit (tmuxSpecDef)
import Uracil.Data.Env (Env, Uracil)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.Yank (Yank(Yank))
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
      Yank ident (Register.Special "*") RegisterType.Line

yankMenuSpec :: [Text] -> Uracil ()
yankMenuSpec chars = do
  setL @Env Env.yanks items
  setCurrentBufferContent ["1", "2", "3"]
  setCurrentLine 1
  withInput (Just 0.05) chars uraYankMenu

yankChars :: [Text]
yankChars =
  ["k", "k", "k", "y"]

yankMenuYankSpec :: Uracil ()
yankMenuYankSpec = do
  yankMenuSpec yankChars
  gassertEqual (Left (NonEmpty.toList targetItem)) =<< getreg unnamedRegister

test_yankMenuYank :: IO ()
test_yankMenuYank =
  tmuxSpecDef yankMenuYankSpec

pasteChars :: [Text]
pasteChars =
  ["k", "k", "k", "p"]

yankMenuPasteSpec :: Uracil ()
yankMenuPasteSpec = do
  yankMenuSpec pasteChars
  gassertEqual (["1", "2"] <> NonEmpty.toList targetItem <> ["3"]) =<< currentBufferContent

test_yankMenuPaste :: IO ()
test_yankMenuPaste =
  tmuxSpecDef yankMenuPasteSpec

ppasteChars :: [Text]
ppasteChars =
  ["k", "k", "k", "P"]

yankMenuPpasteSpec :: Uracil ()
yankMenuPpasteSpec = do
  yankMenuSpec ppasteChars
  gassertEqual (["1"] <> NonEmpty.toList targetItem <> ["2", "3"]) =<< currentBufferContent

test_yankMenuPpaste :: IO ()
test_yankMenuPpaste =
  tmuxSpecDef yankMenuPpasteSpec
