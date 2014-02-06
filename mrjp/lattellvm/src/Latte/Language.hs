module Latte.Language (latteDef) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

latteDef :: LanguageDef st
latteDef = emptyDef {
  commentStart = "/*",
  commentEnd = "*/",
  commentLine = "//",
  nestedComments = False,
  identStart = letter <|> oneOf "_$",
  identLetter = alphaNum <|> oneOf "_$",
  opStart = opLetter latteDef,
  opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
  reservedOpNames = [],
  reservedNames = ["class", "else", "false", "for", "if", "new", "null", 
    "return", "true", "while"],
  caseSensitive = True
}
