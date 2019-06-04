{-# LANGUAGE DeriveAnyClass #-}

module Uracil.Data.Error where

import Chiasma.Data.TmuxError (TmuxError)
import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Orphans ()

import Uracil.Data.YankError (YankError)

data Error =
  Rpc RpcError
  |
  Decode DecodeError
  |
  Persist PersistError
  |
  Tmux TmuxError
  |
  Mapping MappingError
  |
  Setting SettingError
  |
  Yank YankError
  deriving (Show, Generic, ReportError)

deepPrisms ''Error
