module AiScript.Printer
  ( printer
  ) where

import           AiScript.Syntax
import qualified Data.Double.Conversion.Text as Double
import qualified Data.List                   as List
import           Madlib.Operator
import           Madlib.Pretty
import qualified Prettyprinter               as P

printBool :: Bool -> P.Doc ann
printBool bool =
  if bool then "true" else "false"

printExpr :: Expr -> P.Doc ann
printExpr = \case
  ENull ->
    "null"

  ENumber n ->
    if n < 0 then
      P.parens $ P.pretty $ Double.toShortest n
    else
      P.pretty $ Double.toShortest n

  EString str ->
    P.dquotes (P.pretty str)

  EBool bool ->
    printBool bool

  EArray es ->
    P.brackets $
      List.map printExpr es
        |> join " "

  EObject objs ->
    let
      printObj :: Object -> P.Doc ann
      printObj obj =
        P.pretty obj.name
          |> add ": "
          |> add (printExpr obj.value)
    in
    P.braces $
      objs
        |> List.map printObj
        |> join P.line

  EIndex e index ->
    printExpr e
      |> add (P.brackets $ printExpr index)

  ENot e@EBinOp{} ->
    "!"
      |> add (P.parens $ printExpr e)

  ENot e@ENot{} ->
    "!"
      |> add (printExpr e)

  ENot e ->
    P.parens
      ( "!"
          |> add (printExpr e)
      )

  ETemplate templates ->
    "`"
      |> add
        ( templates
            |> List.foldl'
              (\acc -> \case
                TString str ->
                  acc
                    |> addPretty str

                TExpr e ->
                  acc
                    |> add (P.braces $ printExpr e)
              )
              P.emptyDoc
        )
      |> add "`"

  EVar name ->
    P.pretty name

  EApp e args ->
    printExpr e
      |> add
        ( args
            |> map printExpr
            |> join P.space
            |> P.parens
        )

  EProp e prop ->
    printExpr e
      |> add "."
      |> addPretty prop

  ENamespace e name ->
    printExpr e
      |> add ":"
      |> addPretty name

  EIf body -> do
    "if"
      |> addSpace (printExpr body.then_.cond)
      |> addSpace (printStatement body.then_.body)
      |> add
        ( case body.elifs of
            [] ->
              ""

            xs ->
              P.space
                |> add
                  ( map
                      (\then_ ->
                        "elif"
                          |> addSpace (printExpr then_.cond)
                          |> addSpace (printStatement then_.body)
                      )
                      xs
                    |> join P.space
                  )
        )
      |> add
        ( body.else_
            |> maybe ""
              (\s ->
                P.space
                  |> add "else"
                  |> addSpace (printStatement s)
              )
        )

  EMatch match ->
    let
      printCond :: MatchCond -> P.Doc ann
      printCond = \case
        MatchAny ->
          "*"

        MatchExpr e ->
          printExpr e

      printMatch :: (MatchCond, Statement) -> P.Doc ann
      printMatch (cond, body) =
        printCond cond
          |> add "=>"
          |> add (printStatement body)
    in
    "match"
      |> addSpace (printExpr match.cond)
      |> addSpace
        ( P.braces $
            match.matches
              |> List.map printMatch
              |> join P.line
        )

  EBinOp body ->
    P.parens $
      printExpr body.lhs
        |> addSpace (P.pretty body.op)
        |> addSpace (printExpr body.rhs)

  ELambda body ->
    "@"
      |> add
        ( body.args
            |> List.map P.pretty
            |> join P.space
            |> P.parens
        )
      |> add (printStatement body.body)

  EEval body ->
    "eval"
      |> add
        ( body
            |> map printStatement
            |> join P.line
            |> P.braces
        )

  ESequence body ->
    body
      |> List.map printStatement
      |> join P.line

  EParen e ->
    P.parens $ printExpr e

printStatement :: Statement -> P.Doc ann
printStatement = \case
  SExpr e ->
    printExpr e

  SVarDef vardef name e  ->
    let
      defType =
        case vardef of
          VLet ->
            "let"

          VVar ->
            "var"
    in
    defType
      |> addSpace (P.pretty name)
      |> add "="
      |> add (printExpr e)

  SFunDef name body ->
    "@"
      |> addPretty name
      |> add
          ( P.parens $
              body.args
                |> List.map P.pretty
                |> join P.space
          )
      |> add (printStatement body.body)

  SAssign op l r ->
    printExpr l
      |> addPretty op
      |> add (printExpr r)

  SReturn e ->
    "return"
      |> addSpace (printExpr e)

  SFor forBody ->
    let
      printForLet :: ForLet -> P.Doc ann
      printForLet forLet =
        ""
          |> addSpace "let"
          |> addSpace (P.pretty forLet.name)
          |> add
            ( forLet.initValue
                |> maybe ""
                  (\e ->
                    "="
                      |> add (printExpr e)
                  )
            )
    in
    "for"
      |> add (maybe "" printForLet forBody.let_)
      |> addSpace (printExpr forBody.loopCount)
      |> addSpace (printStatement forBody.body)

  SEach eachBody ->
    "each"
      |> addSpace
        ( "let"
            |> addSpace (P.pretty eachBody.let_)
        )
      |> addSpace (printExpr eachBody.array)
      |> addSpace (printStatement eachBody.body)

  SLoop body ->
    "loop"
      |> addSpace (printStatement body)

  SBlock [s] ->
    P.braces $ printStatement s

  SBlock ss ->
    P.braces $
      ss
        |> map printStatement
        |> join P.line

  SBreak ->
    "break"

  SContinue ->
    "continue"

printer :: Program -> P.Doc ann
printer p =
  p
    |> map printStatement
    |> join P.line
