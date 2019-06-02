{-# LANGUAGE DeriveAnyClass #-}

module Uracil.Data.RegEvent where

import Uracil.Data.Register (Register)
import Uracil.Data.RegisterType (RegisterType)

data RegEvent =
  RegEvent {
    inclusive :: Bool,
    operator :: Text,
    regcontents :: [Text],
    regname :: Register,
    regtype :: RegisterType
  }
  deriving (Eq, Show, Generic, MsgpackDecode)
