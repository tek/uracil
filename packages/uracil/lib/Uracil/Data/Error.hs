module Uracil.Data.Error where

import Chiasma.Data.TmuxError (TmuxError)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
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
