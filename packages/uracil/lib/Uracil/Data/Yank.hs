module Uracil.Data.Yank where

import Chiasma.Data.Ident (Ident, Identifiable (..))
import Control.Lens (makeClassy)
import qualified Data.List.NonEmpty as NonEmpty
import Prettyprinter (Pretty (pretty), nest, vsep, (<+>))
import Ribosome (Register, RegisterType)

import Uracil.Data.YankCommand (YankCommand (YankCommand))

data Yank =
  Yank {
    _ident :: Ident,
    _register :: Register,
    _regtype :: RegisterType,
    _operator :: YankCommand,
    _content :: NonEmpty Text
  }
  deriving stock (Eq, Show)

makeClassy ''Yank

instance Identifiable Yank where
  identify = _ident

instance Pretty Yank where
  pretty (Yank _ r rt (YankCommand op) t) =
    nest 2 (vsep (header : (pretty <$> NonEmpty.toList t)))
    where
      header =
        "yank:" <+> pretty r <+> pretty rt <+> pretty op

newtype YankDup =
  YankDup Yank
  deriving stock (Eq, Show)

instance Ord YankDup where
  compare (YankDup l) (YankDup r) =
    (comparing _regtype <> comparing _content) l r
