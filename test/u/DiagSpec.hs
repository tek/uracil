{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec (htf_thisModulesTests) where

import Control.Monad.IO.Class (liftIO)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Test.Unit (withLog)
import Test.Framework

import Unit (specDef)
import Uracil.Data.Env (Uracil)
import Uracil.Diag (uraDiag)

target :: [Text]
target = [
  "# Diagnostics",
  "",
  "## Errors"
  ]

diagSpec :: Uracil ()
diagSpec = do
  uraDiag
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  specDef (withLog diagSpec)
