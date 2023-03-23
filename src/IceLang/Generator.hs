module IceLang.Generator
  ( gen
  ) where

import qualified AiScript.Syntax as IS
import qualified Data.List       as List
import           Madlib.Operator

import           IceLang.Syntax

genExpr :: ExprL -> IS.Expr
genExpr (Located _ expr) =
  case expr of
    ENull ->
      IS.ENull

    EInt n ->
      IS.ENumber (fromIntegral n)

    EFloat f ->
      IS.ENumber f

    EString s ->
      IS.EString s

    EBool b ->
      IS.EBool b

    ETemplate ts ->
      let
        genTemplate = \case
          TString s ->
            IS.TString s
          TExpr e ->
            IS.TExpr $ genExpr e
      in
      IS.ETemplate $ List.map genTemplate ts

    EVar x ->
      IS.EVar x

    EProp l name ->
      IS.EProp (genExpr l) name

    ENamespace l name ->
      IS.ENamespace (genExpr l) name

    EIndex l index ->
      IS.EIndex (genExpr l) (genExpr index)

    EArray vs ->
      IS.EArray $ List.map genExpr vs

    EObject fields ->
      let
        genField (name, v) =
          IS.Object name (genExpr v)

      in
      IS.EObject $ List.map genField fields

    EBinOp op l r ->
      IS.EBinOp $ IS.BinOpBody op (genExpr l) (genExpr r)

    ENot e ->
      IS.ENot (genExpr e)

    EAbs args body ->
      IS.ELambda $ IS.FunBody args (IS.SBlock $ List.map genStatement body)

    ECall e params ->
      IS.EApp (genExpr e) (List.map genExpr params)

    EIf then_ thenBody elifs else_ ->
      let
        genThen (cond, body) =
          IS.Then (genExpr cond) (genStatement body)
      in
      IS.EIf $
        IS.IfBody
          (genThen (then_, thenBody))
          (List.map genThen elifs)
          (fmap genStatement else_)

    EMatch cond pats ->
      let
        genMatch (pat, body) =
          case pat of
            PatAny ->
              (IS.MatchAny, genStatement body)

            PatExpr e ->
              (IS.MatchExpr (genExpr e), genStatement body)
      in
      IS.EMatch $
        IS.Match (genExpr cond) (List.map genMatch pats)

    EEval body ->
      IS.EEval (List.map genStatement body)

genStatement :: StatementL -> IS.Statement
genStatement (Located _ statement) =
  case statement of
    SExpr e ->
      IS.SExpr $ genExpr e

    SLet name v ->
      IS.SVarDef IS.VLet name (genExpr v)

    SVar name v ->
      IS.SVarDef IS.VVar name (genExpr v)

    SAssign op l r ->
      IS.SAssign op (genExpr l) (genExpr r)

    SReturn e ->
      IS.SReturn (genExpr e)

    SFunDef name args body ->
      IS.SFunDef name $
        IS.FunBody args (IS.SBlock $ List.map genStatement body)

    SFor let_ e body ->
      let
        genForLet (name, v) =
          IS.ForLet name (fmap genExpr v)
      in
      IS.SFor $
        IS.ForBody
          (fmap genForLet let_)
          (genExpr e)
          (genStatement body)

    SEach let_ e body ->
      IS.SEach $
        IS.EachBody let_ (genExpr e) (genStatement body)

    SLoop body ->
      IS.SLoop (genStatement body)

    SBlock ss ->
      IS.SBlock $ List.map genStatement ss

    SBreak ->
      IS.SBreak

    SContinue ->
      IS.SContinue

gen :: Program -> IS.Program
gen p =
  p.toplevels
    |> List.map genStatement
