module Uracil.Settings where

import Ribosome.Data.Setting (Setting(Setting))

pasteTimeout :: Setting Int
pasteTimeout =
  Setting "paste_timeout" True (Just 1)

skipYank :: Setting Bool
skipYank =
  Setting "skip_yank" True (Just False)
