module Uracil.Settings where

import Ribosome.Data.Setting (Setting (Setting))
import Time (Seconds)

pasteTimeout :: Setting Seconds
pasteTimeout =
  Setting "paste_timeout" True (Just 1)

skipYank :: Setting Bool
skipYank =
  Setting "skip_yank" True (Just False)
