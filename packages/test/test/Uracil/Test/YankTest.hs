module Uracil.Test.YankTest where

import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Window (setCurrentLine)
import Ribosome.Control.Ribosome (newRibosome)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Test.Embed (integrationTestDef)
import Ribosome.Test.Run (UnitTest)

import Uracil.Plugin (plugin')
import Uracil.Test.Unit (UracilTest)

lines' :: [Text]
lines' =
  "very very very very very very very very very very very very very very very very very long line" : rest
  where
    rest =
      l <$> [(1 :: Int)..10]
    l i =
      "line " <> show i

yankTest :: UracilTest ()
yankTest = do
  setCurrentBufferContent lines'
  setCurrentLine 1
  vimCommand "1yank"
  sleep 0.2

test_yank :: UnitTest
test_yank = do
  ribo <- newRibosome "ura" def
  integrationTestDef (plugin' ribo) yankTest
