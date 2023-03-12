{-# LANGUAGE DuplicateRecordFields #-}

module TypedAST where

import qualified Data.Map as Map
import Syntax (CompilationUnit)
import qualified Syntax as Untyped

type Name = String

data Type
  = FloatType
  | FuncType FuncSignature
  | UnknownType
  deriving (Show)

data FuncSignature = FuncSignature {returnType :: Type, params :: Map.Map String Type}
  deriving (Show)

-- type Param = (String, Type)

data Instr
  = Float Double
  | BinOp Untyped.Op Instr Instr
  | GetParam String Type
  | GetFunc String Type
  | Call {target :: Instr, args :: [Instr]}
  | UnknownInstr
  deriving (Show)

-- \| GetLocal String Type
-- \| SetLocal String Expr

-- | A typed function in Kaleidoscope.
data Func = Func
  { name :: Name,
    sig :: FuncSignature,
    locals :: Map.Map String Type,
    blocks :: Map.Map String BasicBlock
  }
  deriving (Show)

data BasicBlock = BasicBlock
  { name :: String,
    instrs :: [Instr]
  }
  deriving (Show)

data Defn
  = FuncDefn Func
  | ExternDefn Name FuncSignature
  deriving (Show)

newtype CompilationUnit = CompilationUnit {defns :: Map.Map String Defn}
  deriving (Show)
