{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypedAST where

import qualified Data.Map as Map
import qualified Syntax as Untyped
import Text.PrettyPrint

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
  | -- | JumpIfTrue (returnType, cond, blockNameIfTrue, blockNameIfFalse)
    JumpIfTrue Type Instr String String
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
  | -- | an instruction that will run within `main`, it's at the top level.
    MainInstr Instr
  deriving (Show)

data CompilationUnit = CompilationUnit
  { defns :: Map.Map String Defn,
    mainInstrs :: [Instr]
  }
  deriving (Show)

ppCompilationUnit :: CompilationUnit -> Doc
ppCompilationUnit (CompilationUnit {defns, mainInstrs}) =
  let mainInstrsDoc = text "Main Instrs:" $$ vcat (map ppInstr mainInstrs)
      defnsDoc = text "Defns:" $$ vcat (map ppDefn (Map.toList defns))
   in mainInstrsDoc $$ defnsDoc

ppDefn :: (String, Defn) -> Doc
ppDefn (_, FuncDefn (Func {name, sig, locals, blocks})) =
  text "function" <+> text name <+> ppFuncSignature sig $$ vcat (map (nest 1 . ppBasicBlock) (Map.toList blocks))
ppDefn (_, ExternDefn name sig) =
  text "extern" <+> text name <+> ppFuncSignature sig
ppDefn (_, MainInstr instr) =
  text "MainInstr" $$ ppInstr instr

ppFuncSignature :: FuncSignature -> Doc
ppFuncSignature (FuncSignature {returnType, params}) =
  text "(" <+> vcat (map ppParam (Map.toList params)) <+> text ")"

ppParam :: (String, Type) -> Doc
ppParam (name, type_) =
  text name <+> text ":" <+> ppType type_

ppType :: Type -> Doc
ppType FloatType = text "float"
ppType (FuncType sig) = text "fn" <+> ppFuncSignature sig
ppType UnknownType = text "<unknown>"

ppBasicBlock :: (String, BasicBlock) -> Doc
ppBasicBlock (_, BasicBlock {name, instrs}) =
  let body = nest 1 (vcat (map ppInstr instrs))
   in (text "basic block" <+> quotes (text name)) $$ nest 1 (braces body)

ppInstr :: Instr -> Doc
ppInstr (Float value) = double value
ppInstr (BinOp op left right) =
  text "BinOp"
    <+> parens
      ( text "op="
          <+> ppOp op
          <+> text ", left="
          <+> ppInstr left
          <+> text ", right="
          <+> ppInstr right
      )
ppInstr (GetParam name _) = text "GetParam" <+> text name
ppInstr (GetFunc name _) = text "GetFunc" <+> text name
ppInstr (Call {target, args}) =
  text "Call" <+> ppInstr target $$ parens (vcat (map ppInstr args))
ppInstr (JumpIfTrue _ cond then_ else_) =
  text "JumpIfTrue"
    $$ parens
      ( text "cond="
          <+> ppInstr cond
          <+> text ", then="
          <+> text then_
          <+> text ", else="
          <+> text else_
      )
ppInstr UnknownInstr = text "<unknown instruction>"

ppOp :: Untyped.Op -> Doc
ppOp Untyped.Plus = text "+"
ppOp Untyped.Minus = text "-"
ppOp Untyped.Times = text "*"
ppOp Untyped.Divide = text "/"
ppOp Untyped.LessThan = text "<"
ppOp Untyped.GreaterThan = text ">"
