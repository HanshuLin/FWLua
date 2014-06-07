module ParserTD (
  parseExp,
  printParseTree
) where
import SymTab
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import System.Environment

transOp s = case s of
  "+"   -> Plus
  "-"   -> Minus
  "*"   -> Times
  "/"   -> Divide
  ">="  -> Ge
  ">"   -> Gt
  "<="  -> Le
  "<"   -> Lt
  "=="  -> Eq
  "~="  -> Nq
  "and" -> And
  "or"  -> Or
  "%"   -> Mod
  "^"   -> Power
  ".."  -> Cont
  o     -> error $ "Unexpected operator " ++ o
  
transUnop s = case s of
  "-"   -> Neg
  "not" -> Not
  "#"   -> Num
  o     -> error $ "Unexpected operator " ++ o

fileP :: GenParser Char st Statement
fileP = do
  prog <- sequenceStatement
  eof
  return prog

-- STATEMENT ---------------------------------------------------------

sequenceStatement = do
  spaces
  st <- singleStatement
  spaces
  char ';'
  rest <- optionMaybe restSeq
  return (case rest of
    Nothing   -> st
    Just st' -> Seq st st')

restSeq = do
  sequenceStatement

singleStatement = assignmentStatement
              <?> "condition, loop or assignment statement"

assignmentStatement = do
  name <- varname
  spaces
  char '='
  spaces
  expr <- expression
  return $ Assign name expr

varname = do
  s <- many $ alphaNum
  return s

-- TABLES -------------------------------------------------------------

tableSequenceStatement = do
  spaces
  st <- tableAssignStatement
  spaces
  char ';'                       -- SUGER
  spaces
  rest <- optionMaybe restTab
  return (case rest of
    Nothing   -> st
    Just st' -> Seq st st')
    
tableAssignStatement = assignmentStatement
    
restTab = do
  tableSequenceStatement



-- EXPRESSION ---------------------------------------------------------

expression = tableConstructor
         <|> try(unopExp)
         <|> binopExp

         <?> "expression"

tableConstructor = do
  spaces
  char '{'
  spaces
  stat <- tableSequenceStatement
  spaces
  char '}'
  return (Tconst stat)


prefixExp = valueTerm
        <|> parenExpression
        <|> try(variable)
        <|> globalVariable
        <?>"term or parenExpression"
         
valueTerm = do
  t <- numberTerm
   <|> try(booleanTerm)
   <|> try(stringTerm)
  return $ Val t
numberTerm = do
  num <- many1 digit
  return $ VInt $ read num
booleanTerm = do
  bStr <- string "true" <|> string "false" <|> string "skip"
  return $ case bStr of
    "true" -> VBool True
    "false" -> VBool False
    "skip" -> VBool False
stringTerm = do 
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ VStr str

parenExpression = do
  char '('
  spaces
  expr <- expression
  spaces
  char ')'
  return expr

globalVariable = do
  t <- many $ alphaNum
  return $ Var t Nil


variable = do
  t <- tblVar
  rest <- optionMaybe restVar
  case rest of
    Just value -> do
      return $ Var value t
    Nothing    -> return t

tblVar = do
  t <- many $ alphaNum
  char '.'
  k <- many $ alphaNum
  return $ Var k (Tbl t)
  
restVar = do
  char '.'
  k <- many $ alphaNum
  return k

unopExp = do
  nop <- unopSym
  spaces
  expr <- expression
  return $ Nop (transUnop nop) expr

binopExp = do
  spaces
  expr <- level2Expression
  spaces
  rest <- optionMaybe relationSym
  return (case rest of 
    Nothing -> expr
    Just (op, expr') -> Op expr (transOp op) expr')
    
level2Expression = do
  spaces
  expr <- level1Expression
  spaces
  add <- optionMaybe augmentSym
  case add of
    Nothing -> return expr
    Just (op, expr') -> return $ Op expr (transOp op) expr'


  
level1Expression = do
  spaces
  expr <- prefixExp
  spaces
  add <- optionMaybe multipleSym
  case add of
    Nothing -> return expr
    Just (op, expr') -> return $ Op expr (transOp op) expr'
  



unopSym = do
  symbol <- string "-"
        <|> string "not"
        <|> string "#"
  return symbol

   
relationSym = do
  symbol <- try (string "<=")
        <|> string "<"
        <|> try (string ">=")
        <|> string ">"
        <|> string "=="
        <|> string "~="
        <?> "binary operator"
  rest <- level2Expression
  return (symbol, rest)

multipleSym = do
  symbol <- string "*"
        <|> string "/"
  rest <- level2Expression
  return (symbol, rest)
  
augmentSym = do
  symbol <- string "+"
        <|> string "-"
  rest <- level2Expression
  return (symbol, rest)





-- Parsing Method
parseExp fileName = do
  p <- parseFromFile fileP fileName
  return p

printParseTree fileName = do
  p <- parseExp fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> do
      putStrLn $ "[PARSE TREE]"
      print exp
      putStrLn $ ""
        


