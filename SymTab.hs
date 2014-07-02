module SymTab (
  Variable(..),
  Value(..),
  Name,
  ErrorMsg,
  Store,
  VType(..),
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
  | Function
  | VReg String
  | VTrue
  | VFalse
  | VInt Integer
  | VBool Bool
  | VStr String
  | VTable Store
  deriving (Show)
  
data Function = VFunc Variable Expression

data Variable = Variable VType Name
  deriving (Show)
  
data Identifier = Identifier VType Name
  deriving (Show)

data Parameter = Parameter VType
  deriving (Show)
  
data Expression =
    Seq Expression Expression
  | Val Value                                   --Constant
  | New
  | Rget Expression Expression
  | Rset Expression Expression Expression
  | Opraw Expression Binop Expression
  -- | Func Expression Expression
  | Funcall Expression Expression
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


