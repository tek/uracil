module Uracil.Data.Yank where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Text.Prettyprint.Doc (Pretty(..), line, nest, vsep, (<+>))

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

instance Pretty Yank where
  pretty (Yank _ r rt t) =
    nest 2 . vsep $ header : (pretty <$> NonEmpty.toList t)
    where
      header =
        "* yank:" <+> pretty r <+> pretty rt
