module Executer (
  excuteFile
) where
import SymTab
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import System.Environment
import Data.IORef
import System.Random

adrs = mkStdGen 30

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



evaluate :: Expression -> Store -> Either ErrorMsg (Value, Store)

evaluate (Seq e1 e2) s = do
  (_, s') <- evaluate e1 s
  result <- evaluate e2 s'
  return result

evaluate (Val v) s = do
  return (v, s)

evaluate New s = do
  address <- allocateAdr 1 s
  s' <- Right $ Map.insert address Map.empty s
  return ((VReg address), s')

evaluate (Rget table key) s = do
  (a, s1) <- evaluate table s
  (k, s') <- evaluate key s1
  t <- pointToTable a s'
  v <- findVar k t
  return (v, s')

evaluate (Rset table key value) s = do
  (a, s1) <- evaluate table s
  (k, s2) <- evaluate key s1
  (v, s3) <- evaluate value s2
  t <- pointToTable a s3
  case k of
    VStr key -> do
      t' <- Right $ Map.insert key v t
      case a of
        VReg reg -> do
          s' <- Right $ Map.insert reg t' s3
          return (a, s')
    otherwise -> Left $ "<ERROR><Rawset> False key type"


evaluate (Opraw exp1 op exp2) s = do
  (v1, s1) <- evaluate exp1 s
  (v2, s') <- evaluate exp2 s1
  v <- applyOp op v1 v2
  return (v, s')

evaluate (Funcall f expr) s = error "TBD"
  -- (expr', s1) <- evaluate f s
  -- (v1, s2) <- evaluate expr s1
  -- (v, s') <- evaluate expr' s2
  -- return (v, s')

pointToTable :: Value -> Store -> Either ErrorMsg Table
pointToTable (VReg reg) s = do
  t <- Right $ Map.lookup reg s
  case t of 
    Just table -> return table
    otherwise -> Left $ "<ERROR> Don't have table [" ++ reg ++ "]"
    
findVar :: Value -> Table -> Either ErrorMsg Value
findVar (VStr k) t = do
  v <- Right $ Map.lookup k t
  case v of
    Just value -> return value
    Nothing -> Left $ "<ERROR> Key [" ++ k ++ "] does not exist in table"
    
findVar _ _ = do
  Left $ "<ERROR> FALSE TYPE"


allocateAdr :: Int -> Store -> Either ErrorMsg Register

allocateAdr num st = do
  address <- Right $ "_#A0" ++ (show num)
  cond <- Right $ Map.notMember address st
  case cond of
    True -> return address
    False -> allocateAdr (succ num) st

-- Executing Method
run :: Expression -> Either ErrorMsg (Value, Store)
run prog = do
  globle <- Right $ Map.insert "_ENV" Map.empty Map.empty
  evaluate prog globle


excuteFile fileName = do
  putStrLn $ "[STORE]"
  case fileName of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> print $ s
        


