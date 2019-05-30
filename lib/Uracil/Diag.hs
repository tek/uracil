module Uracil.Diag where

import Data.Functor (void)
import Data.Text.Prettyprint.Doc (Doc, line, pretty, (<>))
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, inspectErrors)
import Ribosome.Data.Errors (Errors)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchFocus, scratchSyntax)
import Ribosome.Data.Syntax (
  HiLink(..),
  Syntax(Syntax),
  SyntaxItem(..),
  syntaxMatch,
  )
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

import Uracil.Data.Env (Env)

headlineMatch :: SyntaxItem
headlineMatch =
  syntaxMatch "UracilDiagHeadline" "^#\\+ .*"

itemMatch :: SyntaxItem
itemMatch =
  syntaxMatch "UracilDiagItem" "^* .*"

headlineLink :: HiLink
headlineLink =
  HiLink "UracilDiagHeadline" "Title"

itemLink :: HiLink
itemLink =
  HiLink "UracilDiagItem" "Directory"

diagnosticsSyntax :: Syntax
diagnosticsSyntax =
  Syntax items highlights hilinks
  where
    items = [headlineMatch, itemMatch]
    highlights = []
    hilinks = [headlineLink, itemLink]

errorDiagnostics :: Errors -> Doc a
errorDiagnostics errs =
  "## Errors" <> line <> pretty errs

diagnosticsData ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  m (Doc a)
diagnosticsData = do
  errors <- inspectErrors errorDiagnostics
  return $ headline <> line <> line <> errors
  where
    headline = "# Diagnostics"

uraDiag ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  m ()
uraDiag = do
  content <- diagnosticsData
  void $ showInScratch (lines . show $ content) options
  where
    options = scratchFocus $ scratchSyntax [diagnosticsSyntax] $ defaultScratchOptions "ura-diagnostics"
