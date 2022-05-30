module Uracil.Test.YankTest where

import Polysemy.Test (UnitTest)
import Ribosome.Api.Buffer (setCurrentBufferContent)
import Ribosome.Api.Window (setCurrentLine)
import Ribosome.Host.Api.Effect (vimCommand)
import Ribosome.Host.Test.Run (embedTest_)

lines' :: [Text]
lines' =
  "very very very very very very very very very very very very very very very very very long line" : rest
  where
    rest =
      l <$> [(1 :: Int)..10]
    l i =
      "line " <> show i

test_yank :: UnitTest
test_yank = do
  embedTest_ do
    setCurrentBufferContent lines'
    setCurrentLine 1
    vimCommand "1yank"
