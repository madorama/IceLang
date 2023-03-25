module IceLang.Desugar
  ( TemplateValue (..)
  , Pattern (..)
  , Expr (..)
  , Statement (..)
  , Program
  , desugar
  ) where

import           Control.Monad.State.Strict
import qualified Data.List                  as List
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import qualified IceLang.Syntax             as S

data TemplateValue
  = TString Text
  | TExpr Expr
  deriving Show

data Pattern
  = PatAny
  | PatExpr Expr
  deriving Show

type Then =
  (Expr, Statement)

data Expr
  = ENull
  | EInt Integer
  | EFloat Double
  | EString Text
  | EBool Bool
  | ETemplate [TemplateValue]
  | EVar Text
  | EProp Expr Text
  | ENamespace Expr Text
  | EIndex Expr Expr
  | EArray [Expr]
  | EObject [(Text, Expr)]
  | EBinOp Text Expr Expr
  | ENot Expr
  | EAbs [Text] [Statement]
  | ECall Expr [Expr]
  | EIf Then [Then] (Maybe Statement)
  | EMatch Expr [(Pattern, Statement)]
  | EEval [Statement]
  deriving Show

data Statement
  = SExpr Expr
  | SLet Text Expr
  | SVar Text Expr
  | SAssign Text Expr Expr
  | SFunDef Text [Text] [Statement]
  | SFor (Maybe (Text, Maybe Expr)) Expr Statement
  | SEach Text Expr Statement
  | SLoop Statement
  | SBlock [Statement]
  | SReturn Expr
  | SContinue
  | SBreak
  deriving Show

type Program =
  [Statement]

newtype DesugarState = DesugarState
  { varId :: Integer
  }

emptyState :: DesugarState
emptyState = DesugarState
  { varId = 0
  }

type Desugar =
  State DesugarState

genVar :: Desugar Text
genVar = do
  st <- get
  modify' (\s -> s { varId = s.varId + 1 })
  pure $ "__v" <> Text.pack (show st.varId)

desugarExpr :: S.ExprL -> Desugar Expr
desugarExpr (S.Located _ expr) =
  case expr of
    S.ENull ->
      pure ENull

    S.EInt n ->
      pure $ EInt n

    S.EFloat f ->
      pure $ EFloat f

    S.EString s ->
      pure $ EString s

    S.EBool bool ->
      pure $ EBool bool

    S.ETemplate ts ->
      let
        desugarTemplate =
          \case
            S.TString t ->
              pure $ TString t

            S.TExpr e ->
              TExpr
              <$> desugarExpr e
      in
      ETemplate
      <$> mapM desugarTemplate ts

    S.EVar v ->
      pure $ EVar v

    S.EProp l name ->
      EProp
      <$> desugarExpr l
      <*> pure name

    S.EOptChain l chainValue -> do
      v <- genVar
      EEval <$>
        sequence
          [ SLet v
            <$> desugarExpr l
          , SExpr
            <$>
              ( EIf
                <$>
                  ( (EBinOp "!=" (EVar v) ENull ,)
                    <$>
                      case chainValue of
                        S.ChainName name ->
                          pure $ SExpr (EProp (EVar v) name)
                        S.ChainCall params ->
                          SExpr <$>
                            ( ECall (EVar v)
                              <$> mapM desugarExpr params
                            )
                  )
                <*> pure []
                <*> pure (Just $ SExpr ENull)
              )
          ]

    S.ENamespace l name ->
      ENamespace
      <$> desugarExpr l
      <*> pure name

    S.EIndex l index ->
      EIndex
      <$> desugarExpr l
      <*> desugarExpr index

    S.EArray vs ->
      EArray
      <$> mapM desugarExpr vs

    S.EObject fields ->
      let
        desugarField (name, v) =
          (name ,)
          <$> desugarExpr v
      in
      EObject
      <$> mapM desugarField fields

    S.EBinOp "??" l r -> do
      v <- genVar

      EEval <$> sequence
        [ SLet v
          <$> desugarExpr l
        , SExpr . EIf (EBinOp "!=" (EVar v) ENull, SExpr (EVar v)) []
          <$> (Just . SExpr <$> desugarExpr r)
        ]

    S.EBinOp "|>" l (S.Located _ (S.ECall e params)) -> do
      ECall
      <$> desugarExpr e
      <*> mapM desugarExpr (params ++ [l])

    S.EBinOp "|>" l r -> do
      ECall
      <$> desugarExpr r
      <*> mapM desugarExpr [l]

    S.EBinOp op l r ->
      EBinOp op
      <$> desugarExpr l
      <*> desugarExpr r

    S.ENot e ->
      ENot
      <$> desugarExpr e

    S.EAbs args body ->
      EAbs args
      <$> mapM desugarStatement body

    S.ECall e params ->
      ECall
      <$> desugarExpr e
      <*> mapM desugarExpr params

    S.EIf thenCond thenBody elifs else_ ->
      let
        desugarThen (cond, body) =
          (,)
          <$> desugarExpr cond
          <*> desugarStatement body
      in
      EIf
      <$> desugarThen (thenCond, thenBody)
      <*> mapM desugarThen elifs
      <*> mapM desugarStatement else_

    S.EMatch cond patterns ->
      let
        desugarPat =
          \case
            S.PatAny ->
              pure PatAny

            S.PatExpr e ->
              PatExpr
              <$> desugarExpr e

        desugarPattern (pat, body) =
          (,)
          <$> desugarPat pat
          <*> desugarStatement body
      in
      EMatch
      <$> desugarExpr cond
      <*> mapM desugarPattern patterns

    S.EEval body ->
      EEval
      <$> mapM desugarStatement body

desugarStatement :: S.StatementL -> Desugar Statement
desugarStatement (S.Located _ statement) =
  case statement of
    S.SExpr e ->
      SExpr
      <$> desugarExpr e

    S.SLet name v ->
      SLet name
      <$> desugarExpr v

    S.SVar name v ->
      SVar name
      <$> desugarExpr v

    S.SAssign op l r ->
      SAssign op
      <$> desugarExpr l
      <*> desugarExpr r

    S.SFunDef name args body ->
      SFunDef name args
      <$> mapM desugarStatement body

    S.SFor let_ value body ->
      let
        desugarLetValue (letName, initValue) =
          (letName ,)
          <$> mapM desugarExpr initValue
      in
      SFor
      <$> mapM desugarLetValue let_
      <*> desugarExpr value
      <*> desugarStatement body

    S.SEach let_ value body ->
      SEach let_
      <$> desugarExpr value
      <*> desugarStatement body

    S.SLoop body ->
      SLoop
      <$> desugarStatement body

    S.SBlock ss ->
      SBlock
      <$> mapM desugarStatement ss

    S.SReturn e ->
      SReturn
      <$> desugarExpr e

    S.SContinue ->
      pure SContinue

    S.SBreak ->
      pure SBreak

desugar :: S.Program -> Program
desugar p =
  let
    (_, toplevels) =
      List.foldl'
        (\(accState, accStatements) s ->
          let
            (newStatement, newState) =
              runState (desugarStatement s) accState
          in
          (newState, newStatement : accStatements)
        )
        (emptyState, [])
        p.toplevels
  in
  List.reverse toplevels
