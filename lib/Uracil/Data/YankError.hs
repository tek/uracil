module Uracil.Data.YankError where

import Chiasma.Data.Ident (Ident)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log.Logger (Priority(NOTICE, ERROR))

data YankError =
  Empty
  |
  InvalidMenuIndex
  |
  NoSuchYank Ident
  deriving (Eq, Show)

deepPrisms ''YankError

instance ReportError YankError where
  errorReport Empty =
    ErrorReport "yank history is empty" ["YankError.Empty"] NOTICE
  errorReport InvalidMenuIndex =
    ErrorReport "internal error" ["invalid index in yank menu"] ERROR
