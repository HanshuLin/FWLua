module Executer (
  excuteFile
) where
import SymTab
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Error
import System.Environment

import Data.Global

applyOp :: Binop -> Value -> Value -> Either ErrorMsg Value
applyOp Plus (VInt i) (VInt j) = Right $ VInt $ i + j
applyOp Minus (VInt i) (VInt j) = Right $ VInt $ i - j
applyOp Times (VInt i) (VInt j) = Right $ VInt $ i * j
applyOp Divide (VInt i) (VInt j)
              |  j /= 0 = Right $ VInt $ (div i j)
              |  j == 0 = error "[DIVIDE]Don't Divide 0!"
applyOp Gt (VInt i) (VInt j) = Right $ VBool $ i > j
applyOp Ge (VInt i) (VInt j) = Right $ VBool $ i >= j
applyOp Lt (VInt i) (VInt j) = Right $ VBool $ i < j
applyOp Le (VInt i) (VInt j) = Right $ VBool $ i <= j

applyOp Eq (VInt i) (VInt j) = Right $ VBool $ i == j
applyOp Eq (VBool i) (VBool j) = Right $ VBool $ i == j
applyOp Eq (VStr i) (VStr j) = Right $ VBool $ i == j

applyOp Nq (VInt i) (VInt j) = Right $ VBool $ i /= j
applyOp Power (VInt i) (VInt j) = Right $ VInt $ i ^ j
applyOp Mod (VInt i) (VInt j) = Right $ VInt $ i - (div i j)
applyOp Cont (VStr i) (VStr j) = Right $ VStr $ i ++ j
applyOp And (VBool i) (VBool j) = Right $ VBool $ i && j
applyOp Or (VBool i) (VBool j) = Right $ VBool $ i || j

applyOp _ _ _ = Left $ "ERROR: [BOP-EQ]Types are not the same"

applyUnop :: Unop -> Value -> Either ErrorMsg Value
applyUnop Neg (VInt i) = Right $ VInt $ -i
applyUnop Not (VBool i) = Right $ VBool $ not i


evalState :: Statement -> Store -> Either ErrorMsg (Value, Store)

evalState (Seq s1 s2) s = do
  (_, s') <- evalState s1 s
  result <- evalState s2 s'
  return result
  
evalState (Assign var exp) s = do
  (v, s1) <- evalExpr exp s
  s' <- Right $ Map.insert var v s1
  return (v, s')




evalExpr :: Expression -> Store -> Either ErrorMsg (Value, Store)

evalExpr Nil s = do
  return (VNil, s)

evalExpr (Var var table) s = do
  t <- findTable table s
  v <- Right $ Map.lookup var t
  case v of
    Just value -> return (value, s)
    Nothing -> Left $ "ERROR in finding Var"

evalExpr (Val v) s= do
  return (v, s)
  
evalExpr (Op e1 o e2) s = do
  (v1, s1) <- evalExpr e1 s
  (v2, s') <- evalExpr e2 s1
  v <- applyOp o v1 v2
  return (v, s)

evalExpr (Nop op exp) s = do
  (v1, s')<- evalExpr exp s
  v <- applyUnop op v1
  return (v, s)

evalExpr (Tconst tstate) s = do
  (v, t) <- evalState tstate Map.empty
  return ((VTable t), s)


findTable :: Expression -> Store -> Either ErrorMsg Store

findTable Nil s = do
  return s
  
findTable (Tbl tbl) s = do 
  t <- Right $ Map.lookup tbl s
  case t of 
    Just (VTable table) -> return table
    otherwise -> Left $ "ERROR in finding table"
    
findTable (Var name table) s = do
  t  <- findTable table s
  t' <- findTable (Tbl name) t
  return t'


-- Executing Method
run :: Statement -> Either ErrorMsg (Value, Store)
run prog = do 
  globle <- Right $ Map.empty
  evalState prog globle

excuteFile fileName = do
  putStrLn $ "[EXECUTED RESULT]"
  case fileName of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> print $ s
        


