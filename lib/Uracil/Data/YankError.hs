module Uracil.Data.YankError where

import Chiasma.Data.Ident (Ident)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log.Logger (Priority(NOTICE, ERROR, DEBUG))

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
  deriving (Eq, Show)

deepPrisms ''YankError

instance ReportError YankError where
  errorReport EmptyHistory =
    ErrorReport "yank history is empty" ["YankError.EmptyHistory"] NOTICE
  errorReport InvalidMenuIndex =
    ErrorReport "internal error" ["invalid index in yank menu"] ERROR
  errorReport (NoSuchYank ident) =
    ErrorReport "internal error" ["invalid ident in yank menu"] ERROR
  errorReport (InvalidYankIndex index) =
    ErrorReport "internal error" ["invalid index for yank history: " <> show index] ERROR
  errorReport EmptyEvent =
    ErrorReport "invalid data from neovim" ["empty event"] DEBUG
