module Emci.Pretty where

import Data.Text (Text)

import Prettyprinter
import Prettyprinter.Render.String
import Data.Text.Prettyprint.Doc.Render.Terminal

import Emci.Effects.Process

renderPrettyProcLog :: ProcLogItem -> String
renderPrettyProcLog = renderString
  . layoutPretty defaultLayoutOptions
  . prettyProcLog

renderPrettyProcTerm :: ProcLogItem -> Text
renderPrettyProcTerm = renderStrict
  . layoutPretty defaultLayoutOptions
  . prettyProcLog

prettyProcLog :: ProcLogItem -> Doc AnsiStyle
prettyProcLog (ProcLogItem proc typ msg) =
      brackets (pretty proc)
  <+> annotate (colorByType typ) (pretty msg)
  where colorByType ProcLogStdout = color Green
        colorByType ProcLogStderr = color Red
