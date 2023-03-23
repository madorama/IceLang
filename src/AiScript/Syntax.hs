{-# OPTIONS_GHC -Wno-partial-fields #-}

module AiScript.Syntax where

import           Data.Text

data Object = Object
  { name  :: Text
  , value :: Expr
  } deriving (Show, Eq)

data Then = Then
  { cond :: Expr
  , body :: Statement
  } deriving (Show, Eq)

data TemplateValue
  = TString Text
  | TExpr Expr
  deriving (Show, Eq)

data IfBody = IfBody
  { then_ :: Then
  , elifs :: [Then]
  , else_ :: Maybe Statement
  } deriving (Show, Eq)

data BinOpBody = BinOpBody
  { op  :: Text
  , lhs :: Expr
  , rhs :: Expr
  } deriving (Show, Eq)

data FunBody = FunBody
  { args :: [Text]
  , body :: Statement
  } deriving (Show, Eq)

data MatchCond
  = MatchAny
  | MatchExpr Expr
  deriving (Show, Eq)

data Match = Match
  { cond    :: Expr
  , matches :: [(MatchCond, Statement)]
  } deriving (Show, Eq)

data Expr
  = ENull
  | ENumber Double
  | EString Text
  | EBool Bool
  | EArray [Expr]
  | EObject [Object]
  | EIndex Expr Expr
  | ENot Expr
  | ETemplate [TemplateValue]
  | EVar Text
  | EBinOp BinOpBody
  | EProp Expr Text
  | ENamespace Expr Text
  | EApp Expr [Expr]
  | EIf IfBody
  | EMatch Match
  | ELambda FunBody
  | EEval [Statement]
  | ESequence [Statement]
  | EParen Expr
  deriving (Show, Eq)

data VarDef
  = VLet
  | VVar
  deriving (Show, Eq)

data ForLet = ForLet
  { name      :: Text
  , initValue :: Maybe Expr
  } deriving (Show, Eq)

data ForBody = ForBody
  { let_      :: Maybe ForLet
  , loopCount :: Expr
  , body      :: Statement
  } deriving (Show, Eq)

data EachBody = EachBody
  { let_  :: Text
  , array :: Expr
  , body  :: Statement
  } deriving (Show, Eq)

data Statement
  = SExpr Expr
  | SVarDef VarDef Text Expr
  | SFunDef Text FunBody
  | SAssign Text Expr Expr
  | SReturn Expr
  | SFor ForBody
  | SEach EachBody
  | SLoop Statement
  | SBlock [Statement]
  | SBreak
  | SContinue
  deriving (Show, Eq)

type Program =
  [Statement]
