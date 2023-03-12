module Syntax where
import Text.Parsec (SourcePos)

type Name = String

data Expr
  = Float SourcePos Double
  | BinOp SourcePos Op Expr Expr
  | Var SourcePos String
  -- Function call
  | Call SourcePos Name [Expr]
  | Function SourcePos Name [Expr] Expr
  | Extern SourcePos Name [Expr]
  | If SourcePos Expr Expr Expr
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus 
  | Times
  | Divide
  deriving (Eq, Ord, Show)

type CompilationUnit = [Expr]

getPos :: Expr -> SourcePos
getPos (Float pos _) = pos
getPos (BinOp pos _ _ _) = pos
getPos (Var pos _) = pos
getPos (Call pos _ _) = pos
getPos (Function pos _ _ _) = pos
getPos (Extern pos _ _) = pos
getPos (If pos _ _ _) = pos
