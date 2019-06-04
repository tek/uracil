module Uracil.Data.Paste where

import Data.Hourglass (Elapsed)

import Ribosome.Data.Scratch (Scratch)

data Paste =
  Paste {
    _index :: Int,
    _updated :: Elapsed,
    _scratch :: Scratch
  }
  deriving (Eq, Show)

makeClassy ''Paste
