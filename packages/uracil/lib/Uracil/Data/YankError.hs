module Uracil.Data.YankError where

import Chiasma.Data.Ident (Ident)
import Log (Severity (Debug, Error, Info))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

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

instance ToErrorMessage YankError where
  toErrorMessage = \case
    EmptyHistory ->
      ErrorMessage "yank history is empty" ["YankError.EmptyHistory"] Info
    InvalidMenuIndex ->
      ErrorMessage "internal error" ["invalid index in yank menu"] Error
    (NoSuchYank ident) ->
      ErrorMessage "internal error" ["invalid ident in yank menu: " <> show ident] Error
    (InvalidYankIndex index) ->
      ErrorMessage "internal error" ["invalid index for yank history: " <> show index] Error
    EmptyEvent ->
      ErrorMessage "invalid data from neovim" ["empty event"] Debug
