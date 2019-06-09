{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Control.Monad.IO.Class (liftIO)
import Ribosome.Api.Buffer (currentBufferContent)
import Test.Framework

import qualified Ribosome.Data.Register as Register (Register(Special))
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType(Line))
import Unit (specDef)
import Uracil.Data.Env (Env, Uracil)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Diag (uraDiag)

item1 :: NonEmpty Text
item1 =
  "item1" :| []

item2 :: NonEmpty Text
item2 =
  "item2" :| ["item2 cont"]

item3 :: NonEmpty Text
item3 =
  "item3" :| []

target :: [Text]
target = [
  "# Diagnostics",
  "",
  "## Yank History",
  "* yank: \"* v",
  "  item1",
  "* yank: \"* v",
  "  item2",
  "  item2 cont",
  "* yank: \"* v",
  "  item3", "",
  "## Errors"
  ]

yanks :: [Yank]
yanks =
  uncurry item <$> [
    (Ident.Str "1", item1),
    (Ident.Str "2", item2),
    (Ident.Str "3", item3)
    ]
  where
    item ident =
      Yank ident (Register.Special "*") RegisterType.Line

diagSpec :: Uracil ()
diagSpec = do
  setL @Env Env.yanks yanks
  uraDiag
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  specDef diagSpec
