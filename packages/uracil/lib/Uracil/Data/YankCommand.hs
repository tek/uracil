module Uracil.Data.YankCommand where

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)

newtype YankCommand =
  YankCommand { unYankCommand :: Text }
  deriving stock (Eq, Show, Ord)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)
