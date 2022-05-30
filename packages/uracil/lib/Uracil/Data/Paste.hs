module Uracil.Data.Paste where

import Chiasma.Data.Ident (Ident)
import qualified Chronos
import Ribosome.Data.ScratchId (ScratchId)

data Paste =
  Paste {
    ident :: Ident,
    index :: Int,
    updated :: Chronos.Time,
    scratch :: ScratchId,
    visual :: Bool
  }
  deriving stock (Eq, Show)
