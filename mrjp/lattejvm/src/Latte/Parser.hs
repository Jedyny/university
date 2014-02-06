module Latte.Parser (parser) where

import Control.Applicative hiding ((<|>), many)
import Data.Either (partitionEithers)

import Text.Parsec
import Text.Parsec.Expr

import Latte.Lexer
import Latte.Optimizer (mkIfElse) 
import Latte.ProgramTree

parser = whiteSpace *> (lexeme program) <* eof

program = uncurry <$> (mkNode Module <*> pure []) <*> programBody

programBody = partitionEithers <$> many (Left <$> clazz <|> Right <$> method)

clazz = uncurry <$> (mkNode Clazz 
  <*  reserved "class"
  <*> identifier 
  <*> option "" (reserved "extends" *> identifier)
  <*> pure []) <*> braces (clazzBody)
  
clazzBody = partitionEithers <$> many (try (Left <$> field) <|> Right <$> method)
  
field = mkNode Field <*> clazzName <*> identifier <* semi  

method = mkNode Method 
  <*> clazzName
  <*> identifier
  <*> parens (commaSep argument)
  <*> braces (many statement)
  
argument = (,) <$> clazzName <*> identifier

statement = try assignment
  <|> try block
  <|> try declaration
  <|> try decrement
  <|> try increment
  <|> try ifElse
  <|> try forLoop
  <|> try justExpression
  <|> try returnStatement
  <|> try whileLoop
  <|> emptyStatement

assignment = mkNode AssignS 
  <*> var
  <* symbol "=" 
  <*> expression 
  <* semi
  
block = mkNode BlockS <*> (braces $ many statement)

declaration = mkNode DecS <*> clazzName <*> (commaSep declaredItem) <* semi

declaredItem = 
  (,) <$> identifier <*> (optionMaybe $ symbol "=" *> expression)
  
decrement = mkNode PredS <*> var <* symbol "--" <* semi
 
increment = mkNode SuccS <*> var <* symbol "++" <* semi

ifElse = mkNode mkIfElse 
  <*  reserved "if"
  <*> parens expression
  <*> statement
  <*> (optionMaybe (reserved "else" *> statement))
  
justExpression = mkNode ExpS <*> expression <* semi

returnStatement = mkNode ReturnS 
  <*  reserved "return" 
  <*> (optionMaybe expression) 
  <*  semi
  
forLoop = mkNode ForS 
  <* reserved "for" 
  <* symbol "("
  <*> clazzName 
  <*> identifier
  <* colon 
  <*> var
  <* symbol ")"
  <*> statement

whileLoop = mkNode WhileS <* reserved "while" <*> expression <*> statement

emptyStatement = EmptyS <$ semi

expression = buildExpressionParser operatorTable expressionTerms

expressionTerms = try boolE
  <|> try intE
  <|> try stringE
  <|> try nullE
  <|> try callE
  <|> try castE
  <|> try newArrE 
  <|> try newObjE -- have to be after newArrE
  <|> try varE -- have to be after callE
  <|> parens expression 
  
boolE = mkNode BoolE <*> bool

intE = mkNode IntE <*> int32

stringE = mkNode StringE <*> stringLiteral

nullE = mkNode NullE <* reserved "null"

callE = mkNode (CallE AnyT) 
  <*> var
  <*> (parens $ commaSep expression)
  
castE = mkNode CastE <*> parens clazzName <*> expression

varE = mkNode (VarE AnyT) <*> var

newArrE = mkNode NewArrE 
  <* reserved "new" <*> (mkType <$> identifier) <*> brackets expression
  
newObjE = mkNode NewObjE <* reserved "new" <*> clazzName

operatorTable = [ 
  [prefix "!", prefix "-"],
  [binary "*", binary "/", binary "%"],
  [binary "+", binary "-"],
  [binary "<", binary "<=", binary "==", binary ">=", binary ">", binary "!="],
  [binary "&&"],
  [binary "||"]]
                    
binary op = Infix (mkNode (BinaryE AnyT) <*> opSign op) AssocLeft
prefix op = Prefix (mkNode (UnaryE AnyT) <*> opSign op)

var = dotSep1 $ (,) <$> identifier <*> optionMaybe (brackets expression)

bool = reserved "false" *> return False <|> reserved "true" *> return True

int32 = (fromIntegral :: Integer -> Int32) <$> integer

opSign op = reservedOp op *> return op  

clazzName = mkType <$> ((++) <$> identifier <*> (option "" $ symbol "[]"))

mkNode = (<$> getPosition)
