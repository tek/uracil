{-# LANGUAGE AllowAmbiguousTypes #-}

module Uracil.Data.Env where

import Data.DeepLenses (deepLenses)
import Data.Default (Default(def))
import Path (Abs, Dir, Path, absdir)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Orphans ()

import Uracil.Data.Error (Error)
import Uracil.Data.Yank (Yank)

type Uracil a = Ribo Env Error a

data Env =
  Env {
    _yanks :: [Yank],
    _deletes :: [Yank]
  }
  deriving Show

deepLenses ''Env

instance Default Env where
  def = Env def def
