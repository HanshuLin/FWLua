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

tempName = "_ENV"

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
  return ((VTable Map.empty), s)

evaluate (Rget table key) s = do
  (a, s1) <- evaluate table s
  (k, s') <- evaluate key s1
  t <- pointToTable a s'
  v <- findVar k t
  case k of
    VStr str -> do
      tempName <- Right $ str
      return (v, s')
    otherwise -> do
      Left $ "<ERROR><Rawset> False register type"


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
          vt <- Right $ VTable t'
          s' <- Right $ Map.insert reg vt s3
          return (vt, s')
        VTable tbl -> do
          vt <- Right $ VTable t'
          key <- Right $ tempName
          s' <- Right $ Map.insert key vt s3
          return (vt, s')
        otherwise -> do
          Left $ "<ERROR><Rawset> False register type"
    otherwise -> do
      Left $ "<ERROR><Rawset> False key type"


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


evaluateReg :: Expression -> Store -> Either ErrorMsg (Value, Store)
evaluateReg (Val reg) s = do
  return (reg, s)
evaluateReg (Rget table (Val (VStr key))) s = do
  return ((VReg key), s)


pointToTable :: Value -> Store -> Either ErrorMsg Store

pointToTable (VReg reg) s = do
  t <- Right $ Map.lookup reg s
  case t of 
    Just (VTable table) -> return table
    otherwise -> Left $ "<ERROR> Don't have table [" ++ reg ++ "]"
    
pointToTable (VTable tbl) s = do
  return tbl
  

    
findVar :: Value -> Store -> Either ErrorMsg Value

findVar (VStr k) s = do
  v <- Right $ Map.lookup k s
  case v of
    Just value -> return value
    Nothing -> Left $ "<ERROR> Key [" ++ k ++ "] does not exist in table"
    
findVar _ _ = do
  Left $ "<ERROR> FALSE TYPE"


-- Executing Method
run :: Expression -> Either ErrorMsg (Value, Store)
run prog = do
  reg <- Right $ newIORef tempName
  globle <- Right $ Map.insert "_ENV" (VTable Map.empty) Map.empty
  evaluate prog globle


excuteFile fileName = do
  putStrLn $ "[STORE]"
  case fileName of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> print $ s
        


