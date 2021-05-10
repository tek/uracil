module Uracil.Data.YankCommand where

newtype YankCommand =
  YankCommand Text
  deriving (Eq, Show)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)
