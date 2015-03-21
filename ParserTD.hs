module ParserTD (
  parseExp,
  printParseTree
) where
import System.Environment
import SymTab
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Control.Monad.ST
import System.Environment
import Data.List

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
  "un"  -> Unm
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

fileP :: GenParser Char st Expression
fileP = do
  prog <- sequenceExpression
  eof
  return prog

-- EXPRESSION ---------------------------------------------------------

sequenceExpression = do
  spaces
  st <- expression
  spaces
  try (char ';')
  rest <- optionMaybe restSeq
  return (case rest of
    Nothing   -> st
    Just st' -> Seq st st')

restSeq = do
  try(sequenceExpression)

expression = do
  expr <- try(rsetExpression)
      <|> try(specialExpression)
      <|> try(functionDefinition)
      <|> try(tableConst)
      <|> try(binopExp)
      <|> try(functionCall)
      <|> try(rgetExpression)
      <|> try(constantExpression)
      <?> "expression"
  return expr

functionCall = do
  char '('
  spaces
  expr1 <- expression
  spaces
  char ')'
  char '('
  spaces
  expr2 <- expression
  spaces
  char ')'
  spaces
  return $ Funcall expr1 expr2
              
functionDefinition = do
  string "function"
  spaces
  char '('
  arg <- argument
  char ')'
  spaces
  string "return"
  spaces
  expr <- expression
  spaces
  string "end"
  spaces
  return $ Val $ VFunc arg expr
  
argument = specialArg
       <|> normalArg

specialArg = do
   try(char '_')
   str <- many $ alphaNum
   return $ "_" ++ str

normalArg = do
   str <- many $ alphaNum
   return $ str

argExpression = do
   arg <- argument
   return $ Val $ VArg arg
   
   

rsetExpression = do
  string "rawset("
  spaces
  tbl <- expression
  char ','
  spaces
  key <- expression
  char ','
  spaces
  val <- expression
  spaces
  char ')'
  spaces
  return $ Rset tbl key val
  
rgetExpression = do
  string "rawget("
  spaces
  tbl <- expression
  char ','
  spaces
  key <- expression
  spaces
  char ')'
  spaces
  return $ Rget tbl key
  
tableConst = do
  char '{'
  spaces
  char '}'
  spaces
  return New

specialExpression = try(globalVar)
                <|> try(register)

globalVar = do
  string "_ENV"
  spaces
  return $ Val (VReg "_ENV")
register = do
  string "_#TABLE0"
  num <- many1 digit
  spaces
  return $ Val (VReg $ "_#TABLE0" ++ num)

constantExpression = do
  spaces
  t <- numberTerm
   <|> try(booleanTerm)
   <|> try(stringTerm)
   <|> try(nilTerm)
  return $ Val t
numberTerm = do
  num <- many1 digit
  return $ VInt $ read num
booleanTerm = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> VTrue
    "false" -> VFalse
stringTerm = do 
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ VStr str
nilTerm = do
  string "nil"
  return VNil
  
  
         

binopExp = do
  expr <- level2Expression
  spaces
  rest <- optionMaybe relationSym
  return (case rest of 
    Nothing -> expr
    Just (op, expr') -> Opraw expr (transOp op) expr')
    
level2Expression = do
  expr <- level1Expression
  spaces
  add <- optionMaybe augmentSym
  case add of
    Nothing -> return expr
    Just (op, expr') -> return $ Opraw expr (transOp op) expr'
  
level1Expression = do
  expr <- prefixExp
  spaces
  add <- optionMaybe multipleSym
  case add of
    Nothing -> return expr
    Just (op, expr') -> return $ Opraw expr (transOp op) expr'
    
prefixExp = try(rgetExpression)
        <|> try(constantExpression)
        <|> try(functionCall)
        <|> try(specialExpression)
        <|> try(argExpression)
        <?> "prefix expression"

relationSym = do
  symbol <- try (string "<=")
        <|> string "<"
        <|> try (string ">=")
        <|> string ">"
        <|> string "=="
        <|> string "~="
        <|> string "and"
        <|> string "or"
        <|> string "un"
        <?> "binary operator"
  spaces
  rest <- level2Expression
  return (symbol, rest)

multipleSym = do
  symbol <- string "*"
        <|> string "/"
  spaces
  rest <- level2Expression
  return (symbol, rest)
  
augmentSym = do
  symbol <- string "+"
        <|> string "-"
  spaces
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
      putStrLn $ "======[AST FOR EXP]======"
      print exp
      putStrLn $ ""
        


