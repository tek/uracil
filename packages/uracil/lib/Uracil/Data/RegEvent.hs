module Uracil.Data.RegEvent where

import Ribosome.Data.Register (Register)
import Ribosome.Data.RegisterType (RegisterType)
import Uracil.Data.YankCommand (YankCommand)

data RegEvent =
  RegEvent {
    inclusive :: Bool,
    operator :: YankCommand,
    regcontents :: [Text],
    regname :: Register,
    regtype :: RegisterType
  }
  deriving (Eq, Show, Generic, MsgpackDecode)
