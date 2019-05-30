{-# LANGUAGE DeriveAnyClass #-}

module Uracil.Data.Error where

import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.RpcCall (RpcError)

data Error =
  Rpc RpcError
  |
  Decode DecodeError
  |
  Persist PersistError
  |
  Mapping MappingError
  deriving (Show, Generic, ReportError)

deepPrisms ''Error
