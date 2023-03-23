module IceLang.Compile
  ( compile
  ) where

import           Control.Arrow
import qualified Text.Megaparsec.Error as MegaparsecError

import qualified AiScript.Printer      as Printer
import qualified IceLang.Generator     as GenIS
import qualified IceLang.Parser        as Parser
import           Madlib.Operator

compile filepath src = do
  program <-
    Parser.run filepath src
      |> left MegaparsecError.errorBundlePretty

  pure $
    GenIS.gen program
      |> Printer.printer
