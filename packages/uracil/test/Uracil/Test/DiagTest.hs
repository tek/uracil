module Uracil.Test.DiagTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Log (Severity (Error))
import Polysemy.Test (UnitTest, (===))
import Ribosome (Report (Report))
import Ribosome.Api (currentBufferContent)
import qualified Ribosome.Register as Register
import Ribosome.Report (storeReport)

import Uracil.Data.Yank (Yank (Yank))
import Uracil.Diag (uraDiag)
import Uracil.Test.Run (uraTest)

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
  "",
  "* yank: \"* v y",
  "  item1",
  "* yank: \"* v y",
  "  item2",
  "  item2 cont",
  "* yank: \"* v y",
  "  item3",
  "",
  "## Reports",
  "",
  "### yank",
  "* some",
  "  error"
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
      Yank ident (Register.Special "*") Register.Line "y"

test_diag :: UnitTest
test_diag =
  uraTest do
    atomicModify' (#yanks .~ yanks)
    storeReport "yank" (Report "error" ["some", "error"] Error)
    uraDiag
    content <- currentBufferContent
    target === content
