module Uracil.Data.Register where

import Data.Char (isAlpha, isNumber)
import qualified Data.Text as Text (singleton)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Ribosome.Msgpack.Decode (msgpackFromString)

data Register =
  Named Text
  |
  Numbered Text
  |
  Special Text
  |
  Empty
  deriving (Eq, Show)

instance IsString Register where
  fromString "" =
    Empty
  fromString [a] | isAlpha a =
    Named $ Text.singleton a
  fromString [a] | isNumber a =
    Numbered $ Text.singleton a
  fromString a =
    Special $ toText a

instance MsgpackDecode Register where
  fromMsgpack = msgpackFromString "Register"

prettyRegister :: Text -> Doc a
prettyRegister a =
  "\"" <> pretty a

instance Pretty Register where
  pretty (Named a) = prettyRegister a
  pretty (Numbered a) = prettyRegister (show a)
  pretty (Special a) = prettyRegister a
  pretty Empty = "no register"
