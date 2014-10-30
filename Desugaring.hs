module Desugaring (
  desugarExp,
  printRawExp
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

fileP :: GenParser Char st String
fileP = do
  prog <- sequenceStatement
  eof
  return prog

-- EXPRESSION ---------------------------------------------------------
sequenceStatement = do
  spaces
  st <- statement <|> comment
  spaces
  case st of
    "#COMMENT" -> do
      rest <- optionMaybe restSeq
      return (case rest of
        Nothing   -> ""
        Just st' -> st' )
    otherwise -> do
      skipMany $ char ';' <|> newline
      rest <- optionMaybe restSeq
      return (case rest of
        Nothing   -> st ++ ";"
        Just st' -> st ++ ";\n" ++ st' )


restSeq = do
  try(sequenceStatement)
  
statement = do
    state <- try(assignmentStatement)
         <|> try(functionDefinition)
         <|> try(ifStatement)
         <|> try(functionCall)
         <?> "Statement or Comment"
    skipMany $ char ';'
    return state

expression = try(anonymousFunction)
         <|> try(functionCall)
         <|> try(binopExp)
         <|> try(valueExpression)
         <|> try(variableExpression)
         <?> "Expression"
         
block = do 
  stats <- sequenceStatement
  skipMany $ space <|> newline
  ret <- optionMaybe retStatement
  case ret of
    Nothing -> return $ stats
    Just st -> return $ stats ++ "\n" ++ st ++ ";"
              
retStatement = do
  string "return"
  skipMany $ space <|> newline
  exp <- expression
  skipMany $ char ';'
  return $ "return " ++ exp

identExpression = try(tableIdExpression)
              <|> try(directIdExpression)


-- =========================STATEMENT=================================
assignmentStatement = do
  ident <- identExpression
  spaces
  char '='
  spaces
  val <- expression
  spaces
  return $ "rawset(" ++ ident ++ ", " ++ val ++ ")"

comment = do
   char '-'
   char '-'
   spaces
   skipMany $ noneOf "\n"
   return "#COMMENT"
   
funcBodyExpression = do
  expr <- sequenceStatement
  return $ expr ++ "end"
  
ifStatement = do
  string "if"
  skipMany $ space <|> newline
  cond <- expression
  skipMany $ space <|> newline
  string "then"
  skipMany $ space <|> newline
  tr <- block
  skipMany $ space <|> newline
  string "else"
  skipMany $ space <|> newline
  fa <- block
  skipMany $ space <|> newline
  string "end"
  return $ "((((rawget(_METATABLE, \"if\"))("++
        cond ++"))(function() return " ++
        tr ++ " end))(function() return "++ 
        fa ++" end))()"

-- =========================IDENT EXPRESSION=================================
directIdExpression = do
  var <- many $ alphaNum
  return $ "_ENV, " ++ "\"" ++ var ++ "\""
  
tableIdExpression = do
  char '('
  spaces
  table <- variableExpression
  spaces
  string ")[\""
  spaces
  var <- many $ alphaNum
  spaces
  string "\"]"
  return $ table ++ ", \"" ++ var ++ "\""


-- ===============================EXPRESSION=================================


-- --------------------FUNCTION--------------------
functionDefinition = do
  string "function"
  spaces
  funcName <- many $ alphaNum
  char '('
  arg <- many $ alphaNum
  char ')'
  skipMany $ space <|> newline
  body <- sequenceStatement
  return $ "rawset(_ENV, \"" ++ 
           funcName ++
           "\", function(" ++
           arg ++
           ") " ++
           body ++ ")"
           
anonymousFunction = do
  string "function"
  spaces
  char '('
  arg <- many $ alphaNum
  char ')'
  spaces
  body <- funcBodyExpression
  return $ "function(" ++
           arg ++
           ") " ++
           body

functionCall = do
  name <- funcIdent
  char '('
  spaces
  value <- expression
  spaces
  char ')'
  return $ name ++ "(" ++ value ++ ")"

funcIdent = anonyFunction
        <|> nonyFunction
        <?> "FUNCTION"

anonyFunction = do
  char '('
  spaces
  func <- anonymousFunction
  spaces
  char ')'
  return $ "(" ++ func ++ ")"

nonyFunction = do
  expr <- identExpression
  return $ "(rawget(" ++ expr ++ "))"

-- --------------------EXPRESSION--------------------
variableExpression = do
  var <- identExpression
  return $ "rawget(" ++ var ++ ")" 

valueExpression = do
  spaces
  t <- numberTerm
   <|> try(newTerm)
   <|> try(booleanTerm)
   <|> try(stringTerm)
  return $ t
  
newTerm = do
  table <- string "{}"
  return $ table

numberTerm = do
  num <- many1 digit
  return $ num

booleanTerm = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> "true"
    "false" -> "false"

stringTerm = do
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ "\"" ++ str++ "\""
  
binopExp2 = do
  expr <- prefixExp
  spaces
  op <- string "+"
  spaces
  expr' <- prefixExp
  return $ expr ++ " " ++ op ++ " " ++ expr'

binopExp = do
  expr <- prefixExp
  spaces
  rest <- optionMaybe operationSym
  return (case rest of 
    Nothing -> expr
    Just (op, expr') -> expr ++ " " ++ op ++ " " ++ expr')
    
prefixExp = valueExpression
        <|> variableExpression
    

operationSym = do
  symbol <- try (string "<=")
        <|> string "<"
        <|> try (string ">=")
        <|> string ">"
        <|> string "=="
        <|> string "~="
        <|> string "*"
        <|> string "/"
        <|> string "+"
        <|> string "-"
        <?> "binary operator"
  rest <- binopExp
  return (symbol, rest)

-- Parsing Method
desugarExp fileName = do
  p <- parseFromFile fileP fileName
  return p

printRawExp fileName = do
  p <- desugarExp fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> do
      putStrLn $ "======[DESUGARED EXP]======"
      putStr exp
      putStrLn $ ""
      putStrLn $ ""
        


