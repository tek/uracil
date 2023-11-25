module Uracil.Interpreter.InputIdent where

import Chiasma.Data.Ident (Ident (Uuid))
import Conc (interpretAtomic)
import System.Random (newStdGen, random)

interpretInputIdentRandom ::
  Member (Embed IO) r =>
  InterpreterFor (Input Ident) r
interpretInputIdentRandom sem = do
  q <- embed newStdGen
  interpretAtomic q $ runInputSem (Uuid <$> atomicState' (swap . random)) (raiseUnder sem)
