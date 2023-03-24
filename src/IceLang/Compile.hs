module IceLang.Compile
  ( compile
  ) where

import qualified AiScript.Printer      as Printer
import qualified Data.List             as List
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Madlib.Operator
import qualified Text.Megaparsec.Error as MegaparsecError

import qualified IceLang.Desugar       as Desugar
import qualified IceLang.Generator     as GenIS
import qualified IceLang.Lint          as Lint
import qualified IceLang.Parser        as Parser
import           Prettyprinter         (Doc)

compile :: FilePath -> Text -> Either Text (Doc ann)
compile filepath src = do
  case Parser.run filepath src of
    Left err ->
      Left $ Text.pack (MegaparsecError.errorBundlePretty err)

    Right p ->
      case Lint.lint p of
        Just errs ->
          Left $ Text.intercalate "\n" (List.map (Text.pack . show) errs)

        Nothing ->
          Right $
            Desugar.desugar p
              |> GenIS.gen
              |> Printer.printer
