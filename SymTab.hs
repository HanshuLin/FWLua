module SymTab (
  Variable(..),
  Value(..),
  Name,
  ErrorMsg,
  Store,
  VType(..),
  Statement(..),
  Expression(..),
  Unop(..),
  Binop(..),
  Identifier(..),
  Parameter(..),
  ) where
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Unsafe
import Data.IORef
import System.Environment

type ErrorMsg = String

type Store = Map Name Value

type Name = String

data VType = Integer | Boolean | String | Defined
  deriving (Eq, Show)

data Value = 
    VNil
  | VTable Store
  | VInt Integer
  | VFloat Float
  | VBool Bool
  | VStr String
  | VFunc VType Expression
  deriving (Show)

data Variable = Variable VType Name
  deriving (Show)
  
data Identifier = Identifier VType Name
  deriving (Show)

data Parameter = Parameter VType
  deriving (Show)
  
data Statement = 
    Seq Statement Statement
  | Assign Name Expression
  | Funcall
  | Label
  | Break
  | Goto
  | Block Statement Statement
  | While Expression Statement
  | Repeat Expression Statement
  | If Expression Statement
  | For
  | Function
  | Local
  deriving (Show)

data Expression =
    Nil
  | Tbl Name
  | Var Name Expression
  | Val Value
  | Op Expression Binop Expression          -- binop
  | Nop Unop Expression                     -- unop
  | Tconst Statement                        -- table constructor done!

  | Dot
  | Fdef
  deriving (Show)
  
data Unop = 
    Neg      -- -
  | Not      -- not
  | Num   -- #
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Power    -- ^
  | Mod      -- %
  | Cont     -- ..
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  | Eq       -- == :: Int -> Int -> Bool
  | Nq       -- ~= :: Int -> Int -> Bool
  | And
  | Or
  deriving (Show)



symbolTable :: Store
symbolTable = Map.empty


