module Uracil.Data.Yank where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Text.Prettyprint.Doc (Pretty(..), nest, vsep, (<+>))
import Ribosome.Data.Register (Register)
import Ribosome.Data.RegisterType (RegisterType)

import Uracil.Data.YankOperator (YankOperator(YankOperator))

data Yank =
  Yank {
    _ident :: Ident,
    _register :: Register,
    _regtype :: RegisterType,
    _operator :: YankOperator,
    _text :: NonEmpty Text
  }
  deriving (Eq, Show)

makeClassy ''Yank

instance Identifiable Yank where
  identify = _ident

instance Pretty Yank where
  pretty (Yank _ r rt (YankOperator op) t) =
    nest 2 . vsep $ header : (pretty <$> NonEmpty.toList t)
    where
      header =
        "* yank:" <+> pretty r <+> pretty rt <+> pretty op
