module Uracil.Data.YankCommand where

import Ribosome (MsgpackDecode, MsgpackEncode)

newtype YankCommand =
  YankCommand { unYankCommand :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)
