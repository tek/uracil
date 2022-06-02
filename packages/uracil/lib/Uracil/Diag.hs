module Uracil.Diag where

import Control.Lens ((.~))
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Exon (exon)
import Prettyprinter (Doc, line, nest, pretty, vsep)
import Ribosome (
  ErrorMessage (ErrorMessage),
  Errors,
  Handler,
  HandlerTag (GlobalTag, HandlerTag),
  RpcError,
  Scratch,
  StoredError (StoredError),
  defaultScratchOptions,
  resumeHandlerError,
  )
import qualified Ribosome.Errors as Errors
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

tagName :: HandlerTag -> Text
tagName = \case
  GlobalTag -> "global"
  HandlerTag n -> n

storedError :: StoredError -> Doc a
storedError (StoredError (ErrorMessage _ log _) _) =
  case log of
    [] -> mempty
    (h : t) ->
      nest 2 (vsep (pretty <$> ([exon|* #{h}|] : t)))

tagErrors :: HandlerTag -> [StoredError] -> Doc a
tagErrors t errs =
  pretty [exon|### #{tagName t}|] <> line <> vsep (storedError <$> errs)

errorDiagnostics :: Map HandlerTag [StoredError] -> Doc a
errorDiagnostics errs =
  "## Errors" <> line <> line <> vsep (uncurry tagErrors <$> Map.toAscList errs)

formatYank :: Yank -> Doc a
formatYank y =
  "* " <> pretty y

yanksDiagnostics :: [Yank] -> Doc a
yanksDiagnostics yanks =
  "## Yank History" <> line <> line <> vsep (formatYank <$> yanks)

diagnosticsData ::
  Members [Errors, AtomicState Env] r =>
  Sem r (Doc a)
diagnosticsData = do
  errors <- errorDiagnostics <$> Errors.get
  yanks <- atomicGets (yanksDiagnostics . Env.yanks)
  pure (headline <> line <> line <> yanks <> line <> line <> errors)
  where
    headline =
      "# Diagnostics"

uraDiag ::
  Members [Scratch !! RpcError, Errors, AtomicState Env] r =>
  Handler r ()
uraDiag = do
  content <- diagnosticsData
  void (resumeHandlerError (Scratch.show (lines (show content)) options))
  where
    options =
      defaultScratchOptions "ura-diagnostics"
      & #focus .~ True
      & #syntax .~ [diagnosticsSyntax]
