module Uracil.Diag where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Prettyprinter (Doc, line, nest, pretty, vsep)
import Ribosome (
  Handler,
  Report (Report),
  ReportContext,
  Reports,
  RpcError,
  Scratch,
  StoredReport (StoredReport),
  reportContext,
  resumeReport,
  scratch,
  storedReports,
  )
import qualified Ribosome.Scratch as Scratch
import Ribosome.Syntax (HiLink (..), Syntax (Syntax), SyntaxItem (..), syntaxMatch)

import qualified Uracil.Data.Env as Env
import Uracil.Data.Env (Env)
import Uracil.Data.Yank (Yank)

headlineMatch :: SyntaxItem
headlineMatch =
  syntaxMatch "UracilDiagHeadline" "^#\\+ .*"

itemMatch :: SyntaxItem
itemMatch =
  syntaxMatch "UracilDiagItem" "^\\* .*"

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

storedError :: StoredReport -> Doc a
storedError (StoredReport (Report _ log _) _) =
  case log of
    [] -> mempty
    (h : t) ->
      nest 2 (vsep (pretty <$> ([exon|* #{h}|] : t)))

tagErrors :: ReportContext -> [StoredReport] -> Doc a
tagErrors ctx errs =
  pretty [exon|### #{reportContext ctx}|] <> line <> vsep (storedError <$> errs)

errorDiagnostics :: Map ReportContext [StoredReport] -> Doc a
errorDiagnostics errs | null errs =
  mempty
errorDiagnostics errs =
  "## Reports" <> line <> line <> vsep (uncurry tagErrors <$> Map.toAscList errs)

formatYank :: Yank -> Doc a
formatYank y =
  "* " <> pretty y

yanksDiagnostics :: [Yank] -> Doc a
yanksDiagnostics yanks =
  "## Yank History" <> line <> line <> vsep (formatYank <$> yanks)

diagnosticsData ::
  Members [Reports, AtomicState Env] r =>
  Sem r (Doc a)
diagnosticsData = do
  errors <- errorDiagnostics <$> storedReports
  yanks <- atomicGets (yanksDiagnostics . Env.yanks)
  pure (headline <> line <> line <> yanks <> line <> line <> errors)
  where
    headline =
      "# Diagnostics"

uraDiag ::
  Members [Scratch !! RpcError, Reports, AtomicState Env] r =>
  Handler r ()
uraDiag = do
  content <- diagnosticsData
  void (resumeReport (Scratch.show (lines (show content)) options))
  where
    options =
      scratch "ura-diagnostics"
      & #focus .~ True
      & #syntax .~ [diagnosticsSyntax]
