module Uracil.Data.RegEvent where

import Ribosome.Data.Register (Register)
import Ribosome.Data.RegisterType (RegisterType)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)

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
