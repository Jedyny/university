module Latte.Lexer (
  braces,
  brackets,
  colon,
  commaSep,
  dotSep1,
  identifier,
  integer,
  lexeme,
  operator,
  parens,
  reserved,
  reservedOp,
  semi,
  stringLiteral,
  symbol,
  whiteSpace
) where

import Text.Parsec.Combinator (sepBy1)

import qualified Text.Parsec.Token as Parsec

import Latte.Language (latteDef)

latte = Parsec.makeTokenParser latteDef

braces = Parsec.braces latte
brackets = Parsec.brackets latte
colon = Parsec.colon latte
commaSep = Parsec.commaSep latte
identifier = Parsec.identifier latte
integer = Parsec.integer latte
lexeme = Parsec.lexeme latte
operator = Parsec.operator latte
parens = Parsec.parens latte
reserved = Parsec.reserved latte
reservedOp = Parsec.reservedOp latte
semi = Parsec.semi latte
stringLiteral = Parsec.stringLiteral latte
symbol = Parsec.symbol latte
whiteSpace = Parsec.whiteSpace latte

dotSep1 = flip sepBy1 (Parsec.dot latte)
