module IceLang.Compile
  ( compile
  ) where

import           Control.Arrow
import           Madlib.Operator
import qualified Text.Megaparsec.Error as MegaparsecError

import qualified AiScript.Printer      as Printer
import qualified IceLang.Desugar       as Desugar
import qualified IceLang.Generator     as GenIS
import qualified IceLang.Parser        as Parser

compile filepath src = do
  program <-
    Parser.run filepath src
      |> left MegaparsecError.errorBundlePretty

  pure $
    Desugar.desugar program
      |> GenIS.gen
      |> Printer.printer
