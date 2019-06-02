{-# OPTIONS_GHC -F -pgmF htfpp #-}

module YankMenuSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Ribosome.Api.Buffer (currentBufferContent, setCurrentBufferContent)
import Ribosome.Api.Window (setCurrentLine)
import Ribosome.Test.Input (withInput)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Test.Framework

import Uracil.Data.Env (Env, Uracil)
import qualified Uracil.Data.Env as Env (yanks)
import qualified Uracil.Data.Register as Register (Register(Special))
import qualified Uracil.Data.RegisterType as RegisterType (RegisterType(Line))
import Uracil.Data.Yank (Yank(Yank))
import Uracil.YankMenu (uraYankMenu)

targetItem :: [Text]
targetItem =
  ["item4", "item4 cont"]

items :: [Yank]
items =
  uncurry item <$> [
    (Ident.Str "1", ["item1"]),
    (Ident.Str "2", ["item2", "item2 cont"]),
    (Ident.Str "3", ["item3"]),
    (Ident.Str "4", targetItem),
    (Ident.Str "5", ["item5"])
    ]
  where
    item ident =
      Yank ident (Register.Special "*") RegisterType.Line

inputChars :: [Text]
inputChars =
  ["k", "k", "k", "p"]

yankMenuSpec :: Uracil ()
yankMenuSpec = do
  setL @Env Env.yanks items
  setCurrentBufferContent ["1", "2", "3"]
  setCurrentLine 1
  withInput (Just 0.2) inputChars uraYankMenu
  gassertEqual (["1", "2"] <> targetItem <> ["3"]) =<< currentBufferContent

test_yankMenu :: IO ()
test_yankMenu =
  tmuxGuiSpecDef yankMenuSpec
