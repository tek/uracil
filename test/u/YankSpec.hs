{-# OPTIONS_GHC -F -pgmF htfpp #-}

module YankSpec (htf_thisModulesTests) where

import Neovim (Neovim)
import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Input (syntheticInput)
import Ribosome.Api.Window (setCurrentLine)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimCommandOutput, vimFeedkeys)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Unit (withLogAs)
import System.Log.Logger (Priority(DEBUG), setLevel, updateGlobalLogger)
import Test.Framework

import Uracil.Data.Env (Uracil)
import Uracil.Diag (uraDiag)
import Uracil.Plugin (plugin')

lines' :: [Text]
lines' =
  "very very very very very very very very very very very very very very very very very long line" : rest
  where
    rest =
      l <$> [1..10]
    l i =
      "line " <> show i

yankSpec :: ExceptT RpcError (Neovim ()) ()
yankSpec = do
  setCurrentBufferContent lines'
  setCurrentLine 1
  vimCommand "1yank"
  sleep 0.2

test_yank :: IO ()
test_yank = do
  ribo <- newRibosome "ura" def
  integrationSpecDef (plugin' ribo) yankSpec
