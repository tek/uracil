module Uracil.Data.YankOperator where

newtype YankOperator =
  YankOperator Text
  deriving (Eq, Show, IsString, MsgpackDecode, MsgpackEncode)
