module Uracil.Data.Paste where

import Chiasma.Data.Ident (Ident)
import Data.Hourglass (Elapsed)

import Ribosome.Data.Scratch (Scratch)

data Paste =
  Paste {
    _ident :: Ident,
    _index :: Int,
    _updated :: Elapsed,
    _scratch :: Scratch,
    _visual :: Bool
  }
  deriving (Eq, Show)

makeClassy ''Paste
