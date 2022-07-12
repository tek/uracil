module Uracil.Settings where

import Ribosome.Data.Setting (Setting (Setting))
import Time (MilliSeconds, Seconds)

pasteTimeout :: Setting Seconds
pasteTimeout =
  Setting "paste_timeout" True (Just 1)

pasteTimeoutMillis :: Setting MilliSeconds
pasteTimeoutMillis =
  Setting "paste_timeout_millis" True Nothing

skipYank :: Setting Bool
skipYank =
  Setting "skip_yank" True (Just False)
