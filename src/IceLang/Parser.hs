module IceLang.Parser
  ( run
  ) where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import qualified Data.List                      as List
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Void
import           Madlib.Operator
import           Text.Megaparsec                hiding (Label, unexpected)
import           Text.Megaparsec.Char           (char, eol)

import           IceLang.Syntax
import           IceLang.Token

type Source =
  Text

separator :: Parser ()
separator =
  choice
    [ sym ","
    , ws
    ]

objectSep :: Parser ()
objectSep =
  sym ";"
  <|> separator

sym :: Text -> Parser ()
sym s =
  void $ try $ ws *> symbol s <* ws

k :: Text -> Parser ()
k s =
  try $ ws *> keyword s <* ws

requiredEol :: Parser ()
requiredEol =
  void $ some (hws *> eol *> hws)

betweenSpace :: Parser a -> Parser a
betweenSpace p =
  hws *> p <* hws

parseLocated :: Parser a -> Parser (Located a)
parseLocated p = do
  start <- getSourcePos
  a <- p
  end <- getSourcePos
  pure $ Located (SourceSpan start end) a

run :: FilePath -> Source -> Either (ParseErrorBundle Text Void) Program
run =
  runParser $ ws *> parseProgram <* eof

parseProgram :: Parser Program
parseProgram =
  Program
  <$> sepEndBy (hws *> parseStatement <* hws) requiredEol

parseStatement :: Parser StatementL
parseStatement =
  label "statement" $
    choice
      [ parseLocated $
          SLet
          <$> (k "let" *> identifier)
          <*> (sym "=" *> parseExpr)
      , parseLocated $
          SVar
          <$> (k "var" *> identifier)
          <*> (sym "=" *> parseExpr)
      , parseAssign
      , parseLocated $
          SFunDef
          <$> (sym "@" *> identifier)
          <*> (sym "(" *> sepEndBy identifier separator <* sym ")")
          <*> (sym "{" *> sepEndBy (betweenSpace parseStatement) requiredEol <* symbol "}")
      , parseFor
      , parseEach
      , parseLocated $
          SLoop
          <$> (k "loop" *> parseBlockOrStatement)
      , parseLocated $
          SReturn
          <$> (k "return" *> parseExpr)
      , parseLocated $
          SBreak <$ keyword "break"
      , parseLocated $
          SContinue <$ keyword "continue"
      , parseLocated $ SExpr <$> parseExpr
      ]

parseAssign :: Parser StatementL
parseAssign =
  parseLocated $ try $ do
    l <- parseExpr
    op <-
      choice
        [ "+=" <$ sym "+="
        , "-=" <$ sym "-="
        , "=" <$ sym "="
        ]
    SAssign op l <$> parseExpr

parseFor :: Parser StatementL
parseFor =
  parseLocated $ k "for" *>
    choice
      [ SFor
        <$> (sym "("  *> optional (k "let" *> ((,) <$> identifier <*> optional (sym "=" *> parseExpr))))
        <*> (separator *> parseExpr <* sym ")")
        <*> parseBlockOrStatement
      , SFor
        <$> optional (k "let" *> ((,) <$> identifier <*> optional parseExpr))
        <*> (separator *> parseExpr)
        <*> parseBlockOrStatement
      ]

parseEach :: Parser StatementL
parseEach =
  parseLocated $ k "each" *>
    choice
      [ SEach
        <$> (sym "(" *> k "let" *> identifier)
        <*> (separator *> parseExpr <* sym ")")
        <*> parseBlockOrStatement
      , SEach
        <$> (k "let" *> identifier)
        <*> (separator *> parseExpr)
        <*> parseBlockOrStatement
      ]

parseBlockOrStatement :: Parser StatementL
parseBlockOrStatement =
  choice
    [ parseLocated $
        SBlock
        <$> (sym "{" *> sepEndBy (betweenSpace parseStatement) requiredEol <* ws <* symbol "}")
    , parseStatement
    ]

parseExpr :: Parser ExprL
parseExpr =
  let
    binOp op p = do
      start <- getSourcePos
      _ <- p
      end <- getSourcePos
      pure $ \l r -> Located (SourceSpan start end) (EBinOp op l r)

    table =
      [ [ Prefix
            (do
              start <- getSourcePos
              nots <- some (sym "!")
              end <- getSourcePos
              pure $
                if even (List.length nots) then id else Located (SourceSpan start end) . ENot
            )
        ]
      , [ InfixL (binOp "*" (try $ sym "*" <* notFollowedBy (sym "=>")))
        , InfixL (binOp "/" (sym "/"))
        , InfixL (binOp "%" (sym "%"))
        , InfixL (binOp "^" (sym "^"))
        ]
      , [ InfixL (binOp "+" (try $ sym "+" <* notFollowedBy (sym "=")))
        , InfixL (binOp "-" (try $ sym "-" <* notFollowedBy (sym "=")))
        ]
      , [ InfixN (binOp "<=" (sym "<="))
        , InfixN (binOp ">=" (sym ">="))
        , InfixN (binOp "==" (sym "=="))
        , InfixN (binOp "!=" (sym "!="))
        , InfixN (binOp "<" (sym "<"))
        , InfixN (binOp ">" (sym ">"))
        ]
      , [ InfixL (binOp "&&" (sym "&&"))
        ]
      , [ InfixL (binOp "||" (sym "||"))
        ]
      ]
  in
  label "expr" $
    makeExprParser (hws *> parseTerm Nothing <* hws) table

parseTerm :: Maybe ExprL -> Parser ExprL
parseTerm =
  \case
    Just e ->
      let
        pCall =
          parseLocated $
            ECall e
            <$> (symbol "(" *> ws *> sepEndBy parseExpr separator <* ws <* symbol ")")

        pProp =
          parseLocated $
            EProp e
            <$> (sym "." *> identifier)

        pOptChain =
          parseLocated $
            EOptChain e
            <$> (sym "?." *> identifier)

        pIndex =
          parseLocated $
            EIndex e
            <$> (sym "[" *> parseExpr <* ws <* symbol "]")

        pNamespace =
          parseLocated $
            ENamespace e
            <$> (sym ":" *> identifier)
      in
      choice
        [ pProp >>= parseTerm . Just
        , pOptChain >>= parseTerm . Just
        , pNamespace >>= parseTerm . Just
        , pIndex >>= parseTerm . Just
        , pCall >>= parseTerm . Just
        , pure e
        ]

    Nothing ->
      choice
        [ parseAbs
        , parseIf
        , parseMatch
        , parseLocated $
            EEval
            <$> (k "eval" *> sym "{" *> sepEndBy (betweenSpace parseStatement) requiredEol <* ws <* symbol "}")
        , parseLocated $ EVar <$> identifier
        , parseLiteral
        , try $ symbol "(" *> ws *> parseExpr <* ws <* symbol ")"
        ]
        >>= parseTerm . Just

parseAbs :: Parser ExprL
parseAbs =
  parseLocated $
    EAbs
    <$> (try (sym "@" *> sym "(") *> ws *> sepEndBy identifier separator <* sym ")")
    <*> (sym "{" *> sepEndBy (betweenSpace parseStatement) requiredEol <* ws <* symbol "}")

parseIf :: Parser ExprL
parseIf =
  let
    pElif =
      (,)
      <$> (k "elif" *> parseExpr)
      <*> parseBlockOrStatement
  in
  parseLocated $
    EIf
    <$> (k "if" *> parseExpr)
    <*> parseBlockOrStatement
    <*> many pElif
    <*> optional (k "else" *> parseBlockOrStatement)

parseMatch :: Parser ExprL
parseMatch =
  let
    pPat =
      choice
        [ PatAny <$ sym "*"
        , PatExpr <$> parseExpr
        ]
    pPattern =
      (,)
      <$> pPat
      <*> (sym "=>" *> parseBlockOrStatement)
  in
  parseLocated $
    EMatch
    <$> (k "match" *> parseExpr)
    <*> (sym "{" *> sepEndBy pPattern requiredEol <* ws <* symbol "}")

parseLiteral :: Parser ExprL
parseLiteral = do
  choice
    [ parseLocated $ EFloat <$> try decimal
    , parseLocated $ EInt <$> try integer
    , parseLocated $ EBool <$> bool
    , parseLocated $ ENull <$ keyword "null"
    , parseLocated $ EString <$> stringLiteral '"'
    , parseLocated $ EString <$> stringLiteral '\''
    , parseArray
    , parseObject
    , parseTemplate
    ]

parseArray :: Parser ExprL
parseArray =
  parseLocated $
    EArray
    <$> (sym "[" *> sepEndBy parseExpr separator <* ws <* symbol "]")

parseObject :: Parser ExprL
parseObject =
  let
    pObject =
      (,)
      <$> identifier
      <*> (sym ":" *> parseExpr)
  in
  parseLocated $
    EObject
    <$> (sym "{" *> sepEndBy pObject objectSep <* ws <* symbol "}")

parseTemplate :: Parser ExprL
parseTemplate =
  parseLocated $ do
    vs <- char '`' *> many parseTemplateValue <* char '`'
    let
      values =
        vs
          |> List.groupBy (\a b -> case (a, b) of
            (TString _, TString _) ->
              True

            (TExpr _, TExpr _) ->
              True

            _ ->
              False
          )
          |> List.map (\case
            xs@(TString _ : _) ->
              xs
                |> List.map (\case
                  TString s ->
                    s

                  _ ->
                    error "unknown"
                )
                |> Text.intercalate ""
                |> (List.singleton . TString)

            xs ->
              xs
          )
          |> concat
    pure $ ETemplate values

parseTemplateValue :: Parser TemplateValue
parseTemplateValue =
  choice
    [ try parseTemplateEscape
    , TExpr <$> try (char '{' *> ws *> parseExpr <* ws <* char '}')
    , TString . Text.singleton <$> satisfy (\c -> c /= '`' && c /= '{')
    ]

parseTemplateEscape :: Parser TemplateValue
parseTemplateEscape =
  TString
  <$> choice
    [ "{" <$ string "\\{"
    , "}" <$ string "\\}"
    , "`" <$ string "\\`"
    ]
