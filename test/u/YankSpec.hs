{-# OPTIONS_GHC -F -pgmF htfpp #-}

module YankSpec (htf_thisModulesTests) where

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Window (setCurrentLine)
import Ribosome.Control.Ribosome (newRibosome)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Test.Embed (integrationSpecDef)
import Test.Framework

import Uracil.Data.Env (Uracil)
import Uracil.Plugin (plugin')

lines' :: [Text]
lines' =
  "very very very very very very very very very very very very very very very very very long line" : rest
  where
    rest =
      l <$> [(1 :: Int)..10]
    l i =
      "line " <> show i

yankSpec :: Uracil ()
yankSpec = do
  setCurrentBufferContent lines'
  setCurrentLine 1
  vimCommand "1yank"
  sleep 0.2

test_yank :: IO ()
test_yank = do
  ribo <- newRibosome "ura" def
  integrationSpecDef (plugin' ribo) yankSpec
