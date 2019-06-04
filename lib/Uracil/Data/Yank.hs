module Uracil.Data.Yank where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Data.List.NonEmpty (NonEmpty)

import Uracil.Data.Register (Register)
import Uracil.Data.RegisterType (RegisterType)

data Yank =
  Yank {
    _ident :: Ident,
    _register :: Register,
    _regtype :: RegisterType,
    _text :: NonEmpty Text
  }
  deriving (Eq, Show)

makeClassy ''Yank

instance Identifiable Yank where
  identify = _ident
