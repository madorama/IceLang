{-# LANGUAGE ApplicativeDo #-}

module Madlib.Megaparsec.Token where

import           Control.Monad
import qualified Data.HashSet               as HashSet
import qualified Data.Text                  as Text
import qualified Text.Megaparsec            as P
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Madlib.Megaparsec.Language
import           Madlib.Megaparsec.Parser
import           Text.Megaparsec            (Stream (Token))

parens :: Applicative m => (Text.Text -> m close) -> m a -> m a
parens p = P.between (p "(") (p ")")

braces :: Applicative m => (Text.Text -> m close) -> m a -> m a
braces p = P.between (p "{") (p "}")

angles :: Applicative m => (Text.Text -> m close) -> m a -> m a
angles p = P.between (p "<") (p ">")

brackets :: Applicative m => (Text.Text -> m close) -> m a -> m a
brackets p = P.between (p "[") (p "]")

comma :: (Text.Text -> t) -> t
comma p = p ","

colon :: (Text.Text -> t) -> t
colon p = p ":"

semi :: (Text.Text -> t) -> t
semi p = p ";"

dot :: (Text.Text -> t) -> t
dot p = p "."

semiSep :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
semiSep p p2 = P.sepBy p2 $ semi p

semiSep1 :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
semiSep1 p p2 = P.sepBy1 p2 $ semi p

commaSep :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
commaSep p p2 = P.sepBy p2 $ comma p

commaSep1 :: MonadPlus m => (Text.Text -> m sep) -> m a -> m [a]
commaSep1 p p2 = P.sepBy1 p2 $ comma p

ifp :: P.MonadParsec e s m => m a -> m Bool
ifp p = P.option False (True <$ p)

lexeme :: P.MonadParsec e s m => LanguageStyle m -> m a -> m a
lexeme langStyle = L.lexeme $ langStyle.spaceConsumer

symbol :: MonadTextParsec e s m => LanguageStyle m -> P.Tokens s -> m (P.Tokens s)
symbol langStyle = L.symbol $ hws langStyle

keyword :: MonadTextParsec e s m => LanguageStyle m -> Text.Text -> m ()
keyword langStyle word = void $ lexeme langStyle (string word <* P.notFollowedBy (langStyle.identLetter))

reserved :: MonadTextParsec e s m => LanguageStyle m -> m ()
reserved langStyle =
  void $ P.choice $ map (P.try . keyword langStyle) $ HashSet.toList (langStyle.reservedNames)

stringLiteral :: MonadTextParsec e s m => Token s -> m Text.Text
stringLiteral c = char c >> Text.pack <$> P.manyTill L.charLiteral (char c)

ws :: MonadTextParsec e s m => LanguageStyle m -> m ()
ws langStyle =
  let skipLine = L.skipLineComment $ langStyle.commentLine
      skipBlockComment
        | langStyle.nestedComments = L.skipBlockCommentNested
        | otherwise = L.skipBlockComment
      skipBlock = skipBlockComment (langStyle.commentStart) (langStyle.commentEnd)
  in
  L.space (P.skipSome space1) skipBlock skipLine

hws :: MonadTextParsec e s m => LanguageStyle m -> m ()
hws langStyle =
  let skipLine = L.skipLineComment $ langStyle.commentLine
      skipBlockComment
        | langStyle.nestedComments = L.skipBlockCommentNested
        | otherwise = L.skipBlockComment
      skipBlock = skipBlockComment (langStyle.commentStart) (langStyle.commentEnd)
  in
  L.space (P.skipSome hspace1) skipBlock skipLine

identifier :: MonadTextParsec e s m => LanguageStyle m -> m Text.Text
identifier langStyle =
  P.label "identifier" $
    lexeme langStyle $ do
      let
        ident =
          (:) <$> langStyle.identStart <*> P.many (langStyle.identLetter)

      if langStyle.caseSensitive then do
        P.notFollowedBy $ reserved langStyle
        Text.pack <$> ident
      else do
        P.notFollowedBy $ reserved $ langStyle { reservedNames = HashSet.map Text.toLower (langStyle.reservedNames) }
        Text.pack <$> ident

