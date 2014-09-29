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
  st <- singleExpression
  spaces
  char ';'
  rest <- optionMaybe restSeq
  return (case rest of
    Nothing   -> st
    Just st' -> Seq st st')

restSeq = do
  sequenceExpression

singleExpression = expression

expression = try(rsetExpression)

         <|> try(rgetExpression)
         <|> try(funcCall)

         <|> try(funcExpression)
         <|> try(binopExp)
         <|> try(specialExpression)
         <|> try(constantExpression)
         <|> tableConst
         <?> "rawset, rawget, constant or {}"
         
funcCall = do
  spaces
  char '('
  expr1 <- expression
  char ')'
  char '('
  expr2 <- expression
  char ')'
  spaces
  return $ Funcall expr1 expr2
              
funcExpression = do
  spaces
  string "function"
  spaces
  arg <- argument
  spaces
  string "return"
  spaces
  expr <- expression
  spaces
  string "end"
  spaces
  return $ Val $ VFunc arg expr
  
argument = do
  str <- many $ alphaNum
  return $ str

rsetExpression = do
  spaces
  string "rawset"
  char '('
  tbl <- expression
  char ','
  key <- expression
  char ','
  val <- expression
  spaces
  char ')'
  return $ Rset tbl key val
  
rgetExpression = do
  spaces
  string "rawget"
  char '('
  tbl <- expression
  char ','
  key <- expression
  char ')'
  spaces
  return $ Rget tbl key
  
tableConst = do
  spaces
  char '{'
  spaces
  char '}'
  spaces
  return New

specialExpression = globalVar

globalVar = do
  spaces
  string "_ENV"
  spaces
  return $ Val (VReg "_ENV")

constantExpression = do
  spaces
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
    "true" -> VTrue
    "false" -> VFalse
stringTerm = do 
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ VStr str
  
  
         

binopExp = do
  spaces
  expr <- level2Expression
  spaces
  rest <- optionMaybe relationSym
  return (case rest of 
    Nothing -> expr
    Just (op, expr') -> Opraw expr (transOp op) expr')
    
level2Expression = do
  spaces
  expr <- level1Expression
  spaces
  add <- optionMaybe augmentSym
  case add of
    Nothing -> return expr
    Just (op, expr') -> return $ Opraw expr (transOp op) expr'
  
level1Expression = do
  spaces
  expr <- prefixExp
  spaces
  add <- optionMaybe multipleSym
  case add of
    Nothing -> return expr
    Just (op, expr') -> return $ Opraw expr (transOp op) expr'
    
prefixExp = constantExpression
        <|> rgetExpression
    

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
        


