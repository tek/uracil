module Uracil.Data.Env where

import Ribosome.Orphans ()

import Uracil.Data.Error (Error)
import Uracil.Data.Paste (Paste)
import Uracil.Data.Yank (Yank)
import Uracil.Data.YankCommand (YankCommand)

type Uracil a = Ribo Env Error a

data Env =
  Env {
    _yanks :: [Yank],
    _deletes :: [Yank],
    _paste :: Maybe Paste,
    _previousStar :: [Text],
    _skip :: Maybe (NonEmpty Text),
    _commands :: Maybe YankCommand
  }
  deriving Show

deepLenses ''Env

instance Default Env where
  def = Env def def def def def def
