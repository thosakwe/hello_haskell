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

data FuncSignature = FuncSignature {returnType :: Type, params :: [Param]}
  deriving (Show)

type Param = (String, Type)

data Expr
  = Float Double
  | BinOp Untyped.Op Expr Expr
  | GetParam String Type
  deriving (Show)

-- \| GetLocal String Type
-- \| SetLocal String Expr

-- | A typed function in Kaleidoscope.
data Func = Func
  { name :: Name,
    sig :: FuncSignature,
    locals :: [Param],
    blocks :: Map.Map String BasicBlock
  }
  deriving (Show)

data BasicBlock = BasicBlock
  { name :: String,
    instrs :: [Expr]
  }
  deriving (Show)

data Defn
  = FuncDefn Func
  | ExternDefn Name FuncSignature
  deriving (Show)

newtype CompilationUnit = CompilationUnit {defns :: [Defn]}
  deriving (Show)
