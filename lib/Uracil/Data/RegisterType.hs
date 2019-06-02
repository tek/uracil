module Uracil.Data.RegisterType where

import Ribosome.Msgpack.Decode (msgpackFromString)

data RegisterType =
  Character
  |
  Line
  |
  Block
  |
  BlockWidth Int
  |
  Unknown Text
  deriving (Eq, Show)

instance IsString RegisterType where
  fromString "v" =
    Character
  fromString "V" =
    Line
  fromString a@('c' : 'v' : num) =
    Unknown (toText a)
  fromString a =
    Unknown (toText a)

instance MsgpackDecode RegisterType where
  fromMsgpack = msgpackFromString "RegisterType"

instance MsgpackEncode RegisterType where
  toMsgpack Character =
    toMsgpack ("v" :: Text)
  toMsgpack Line =
    toMsgpack ("V" :: Text)
  toMsgpack Block =
    toMsgpack ("b" :: Text)
  toMsgpack (BlockWidth width) =
    toMsgpack ("b" <> show width :: Text)
  toMsgpack (Unknown _) =
    toMsgpack ("" :: Text)
