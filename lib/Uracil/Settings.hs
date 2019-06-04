module Uracil.Settings where

import Ribosome.Data.Setting (Setting(Setting))

pasteTimeout :: Setting Int
pasteTimeout =
  Setting "paste_timeout" True (Just 1)
