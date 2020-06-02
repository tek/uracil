{-# LANGUAGE DeriveAnyClass #-}

module Uracil.Data.RegEvent where

import Ribosome.Data.Register (Register)
import Ribosome.Data.RegisterType (RegisterType)
import Uracil.Data.YankOperator (YankOperator)

data RegEvent =
  RegEvent {
    inclusive :: Bool,
    operator :: YankOperator,
    regcontents :: [Text],
    regname :: Register,
    regtype :: RegisterType
  }
  deriving (Eq, Show, Generic, MsgpackDecode)
