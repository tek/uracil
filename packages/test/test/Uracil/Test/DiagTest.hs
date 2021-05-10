module Uracil.Test.DiagTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Hedgehog ((===))
import Ribosome.Api.Buffer (currentBufferContent)
import qualified Ribosome.Data.Register as Register (Register(Special))
import qualified Ribosome.Data.RegisterType as RegisterType (RegisterType(Line))
import Ribosome.Test.Run (UnitTest)

import Uracil.Data.Env (Env)
import qualified Uracil.Data.Env as Env (yanks)
import Uracil.Data.Yank (Yank(Yank))
import Uracil.Diag (uraDiag)
import Uracil.Test.Unit (UracilTest, testDef)

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
  "* yank: \"* v y",
  "  item1",
  "* yank: \"* v y",
  "  item2",
  "  item2 cont",
  "* yank: \"* v y",
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
      Yank ident (Register.Special "*") RegisterType.Line "y"

diagTest :: UracilTest ()
diagTest = do
  setL @Env Env.yanks yanks
  uraDiag
  content <- currentBufferContent
  target === content

test_diag :: UnitTest
test_diag =
  testDef diagTest
