{-# LANGUAGE DuplicateRecordFields #-}

module TypedAST where

import qualified Data.Map as Map
import qualified Syntax as Untyped

data Type
  = FloatType
  | FuncType FuncSignature

data FuncSignature = FuncSignature {returnType :: Type, params :: [Param]}

type Param = (String, Type)

data Expr
  = Float Double
  | BinOp Untyped.Op Expr Expr
  | GetParam String Type

-- \| GetLocal String Type
-- \| SetLocal String Expr

data Func = Func
  { name :: String,
    sig :: FuncSignature,
    blocks :: Map.Map String BasicBlock
  }

data BasicBlock = BasicBlock
  { name :: String,
    instrs :: [Expr]
  }

data Defn =
  FuncDefn Func
  | ExternDefn FuncSignature
