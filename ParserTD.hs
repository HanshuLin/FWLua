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

singleExpression = rsetExpression
              <?> "condition, loop or assignment statement"

rsetExpression = do
  spaces
  tbl <- tableName
  key <- keyName
  spaces
  char '='
  spaces
  expr <- expression
  return $ Rset tbl key expr
  
tableName = do
  char '('
  spaces
  name <- optionMaybe varName
  spaces
  char ')'
  case name of
    Just "" -> return $ Val (VReg "_ENV")
    Just s -> return $ Val (VReg s)

keyName = do
  char '['
  spaces
  name <- stringTerm
  spaces
  char ']'
  return $ Val name

varName = do
  s <- many $ alphaNum
  return s


expression = valueTerm

         <?> "expression"

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
    "true" -> VTrue
    "false" -> VFalse
stringTerm = do 
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ VStr str


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
        


