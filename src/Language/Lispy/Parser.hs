{-# Language
    FlexibleContexts,
    RecordWildCards
    #-}
module Language.Lispy.Parser where

import Control.Applicative hiding ((<|>), many)
import Data.Functor.Identity

import Text.Parsec
import qualified Text.Parsec.Token as P

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import Language.Lispy.Syntax

lispyDef :: P.GenLanguageDef Text u Identity
lispyDef = P.LanguageDef {
    P.commentStart = "{-",
    P.commentEnd = "-}",
    P.commentLine = "--",
    P.nestedComments = True,
    P.identStart = letter <|> char '_' <|> oneOf ":!$%&*+./<=>?@\\^|-~,;",
    P.identLetter = alphaNum <|> char '_' <|> oneOf ":!#$%&*+./<=>?@\\^|-~,;",
    P.opStart = empty,
    P.opLetter = empty,
    P.reservedNames = ["let", "=", "in", "lambda", "->"],
    P.reservedOpNames = [],
    P.caseSensitive = True
    }

lispy :: P.GenTokenParser Text u Identity
lispy = P.makeTokenParser lispyDef

expr = Identifier <$> name
    <|> Literal <$> literal
    <|> SExpr . V.fromList <$> (char '\'' *> parens (many expr))
    <|> do{ reserved "let"; n <- name; reserved "="; e <- expr; reserved "in"; ctx <- expr; pure $ Let n e ctx}
    <|> do{ reserved "lambda"; args <- many1 name; reserved "->"; body <- expr; pure $ Lambda (V.fromList args) body}
    <|> parens (Apply <$> expr <*> (V.fromList <$> many expr))

name = Name . T.pack <$> identifier

literal = LitString . T.pack <$> stringLiteral
    <|> LitSymbol <$> do{ char '#'; name}
    <|> LitFloating <$> try float
    <|> LitIntegral <$> integer

P.TokenParser{..} = lispy
{-
stringLiteral :: Monad m => ParsecT Text u m String
stringLiteral = P.stringLiteral lispy

float :: Monad m => ParsecT Text u m Double
float = P.float lispy

integer :: Monad m => ParsecT Text u m Integer
integer = P.integer lispy

identifier :: Monad m => ParsecT Text u m String
identifier = P.identifier lispy

-- -}