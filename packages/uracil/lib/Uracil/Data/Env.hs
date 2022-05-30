module Uracil.Data.Env where

import Uracil.Data.Paste (Paste)
import Uracil.Data.Yank (Yank)
import Uracil.Data.YankCommand (YankCommand)

data Env =
  Env {
    yanks :: [Yank],
    deletes :: [Yank],
    paste :: Maybe Paste,
    previousStar :: [Text],
    skip :: Maybe (NonEmpty Text),
    command :: Maybe YankCommand
  }
  deriving stock (Eq, Show, Generic)

instance Default Env where
  def = Env def def def def def def
