module Uracil.Data.RegEvent where

import Ribosome (MsgpackDecode, Register, RegisterType)

import Uracil.Data.YankCommand (YankCommand)

data RegEvent =
  RegEvent {
    inclusive :: Bool,
    operator :: YankCommand,
    regcontents :: [Text],
    regname :: Register,
    regtype :: RegisterType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode)
