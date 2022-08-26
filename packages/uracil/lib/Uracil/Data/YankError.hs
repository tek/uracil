module Uracil.Data.YankError where

import Chiasma.Data.Ident (Ident)
import Log (Severity (Debug, Error, Info))
import Ribosome (Report (Report), Reportable (toReport))

data YankError =
  EmptyHistory
  |
  InvalidMenuIndex
  |
  NoSuchYank Ident
  |
  InvalidYankIndex Int
  |
  EmptyEvent
  deriving stock (Eq, Show)

instance Reportable YankError where
  toReport = \case
    EmptyHistory ->
      Report "yank history is empty" ["YankError.EmptyHistory"] Info
    InvalidMenuIndex ->
      Report "internal error" ["invalid index in yank menu"] Error
    (NoSuchYank ident) ->
      Report "internal error" ["invalid ident in yank menu: " <> show ident] Error
    (InvalidYankIndex index) ->
      Report "internal error" ["invalid index for yank history: " <> show index] Error
    EmptyEvent ->
      Report "invalid data from neovim" ["empty event"] Debug
