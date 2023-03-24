module IceLang.Syntax where

import           Data.Text       (Text)
import           Text.Megaparsec (SourcePos, sourcePosPretty)

data SourceSpan = SourceSpan
  { start :: SourcePos
  , end   :: SourcePos
  } deriving Show

data Located a =
  Located SourceSpan a

instance Show a => Show (Located a) where
  show = \case
    Located s a ->
      "("
      <> show a
      <> ") "
      <> sourcePosPretty s.start
      <> " "
      <> sourcePosPretty s.end

data TemplateValue
  = TString Text
  | TExpr ExprL
  deriving Show

type ExprL =
  Located Expr

data Pattern
  = PatAny
  | PatExpr ExprL
  deriving Show

data Expr
  = ENull
  | EInt Integer
  | EFloat Double
  | EString Text
  | EBool Bool
  | ETemplate [TemplateValue]
  | EVar Text
  | EProp ExprL Text
  | EOptChain ExprL Text
  | ENamespace ExprL Text
  | EIndex ExprL ExprL
  | EArray [ExprL]
  | EObject [(Text, ExprL)]
  | EBinOp Text ExprL ExprL
  | ENot ExprL
  | EAbs [Text] [StatementL]
  | ECall ExprL [ExprL]
  | EIf ExprL StatementL [(ExprL, StatementL)] (Maybe StatementL)
  | EMatch ExprL [(Pattern, StatementL)]
  | EEval [StatementL]
  deriving Show


type StatementL =
  Located Statement

data Statement
  = SExpr ExprL
  | SLet Text ExprL
  | SVar Text ExprL
  | SAssign Text ExprL ExprL
  | SFunDef Text [Text] [StatementL]
  | SFor (Maybe (Text, Maybe ExprL)) ExprL StatementL
  | SEach Text ExprL StatementL
  | SLoop StatementL
  | SBlock [StatementL]
  | SReturn ExprL
  | SContinue
  | SBreak
  deriving Show

newtype Program = Program
  { toplevels :: [StatementL]
  }
  deriving Show
