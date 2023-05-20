module Main where

import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Uracil.Test.ContextPasteTest (test_contextPaste)
import Uracil.Test.DiagTest (test_diag)
import Uracil.Test.PasteTest (test_paste)
import Uracil.Test.YankMenuTest (test_yankMenu)
import Uracil.Test.YankTest (test_yank)

tests :: TestTree
tests =
  testGroup "uracil" [
    unitTest "context paste" test_contextPaste,
    unitTest "diagnostics" test_diag,
    test_paste,
    test_yankMenu,
    unitTest "yank" test_yank
  ]


main :: IO ()
main =
  defaultMain tests
