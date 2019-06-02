module Uracil.Data.Yank where

import Chiasma.Data.Ident (Ident, Identifiable(..))

import Uracil.Data.Register (Register)
import Uracil.Data.RegisterType (RegisterType)

data Yank =
  Yank {
    _ident :: Ident,
    _register :: Register,
    _regtype :: RegisterType,
    _text :: [Text]
  }
  deriving (Eq, Show)

instance Identifiable Yank where
  identify = _ident
