{-# LANGUAGE NamedFieldPuns #-}

-- | A semantic analyzer that turns the untyped AST into a typed one.
module TypedASTPass where

import Control.Monad.State
import KaleidoError
import qualified Syntax as Untyped
import Text.Parsec (SourcePos)
import TypedAST

data CompilerState = CompilerState
  { funcState :: Maybe FuncState,
    result :: CompilerResult
  }
  deriving (Show)

data CompilerResult = CompilerResult
  { compilationUnit :: CompilationUnit,
    errors :: [KaleidoError]
  }
  deriving (Show)

data FuncState = FuncState
  { currentBlockName :: String,
    currentFuncName :: String
  }
  deriving (Show)

runTypedASTPass :: Untyped.CompilationUnit -> CompilerResult
runTypedASTPass untypedExprs =
  -- let _ = map analyzeTopLevelExpr untypedExprs in
  -- let _ = runState analyzeTopLevelExpr untypedExprs in
  let (_, CompilerState {result}) = runState (compileUnit untypedExprs) emptyCompilerState
   in CompilerResult
        { compilationUnit = compilationUnit result,
          errors = errors result
        }

compileUnit :: [Untyped.Expr] -> State CompilerState ()
compileUnit untypedExprs = do
  mapM_ compileTopLevelExpr untypedExprs

compileTopLevelExpr :: Untyped.Expr -> State CompilerState ()
compileTopLevelExpr (Untyped.Function pos name args body) = do
  return ()
compileTopLevelExpr (Untyped.Extern pos name params) = do
  params <- mapM compileParam params
  let sig =
        FuncSignature
          { returnType = FloatType,
            params
          }
  let defn = ExternDefn name sig
  emitDefn defn
compileTopLevelExpr expr = do
  let pos = Untyped.getPos expr
  emitError pos "Not a function or extern."

compileParam :: Untyped.Expr -> State CompilerState (String, Type)
compileParam (Untyped.Var _ name) = return (name, FloatType)
compileParam expr = do
  let pos = Untyped.getPos expr
  emitError pos "Not a var."
  return ("?", UnknownType)

emptyCompilerState :: CompilerState
emptyCompilerState =
  CompilerState {funcState = Nothing, result = emptyCompilerResult}

emptyCompilerResult :: CompilerResult
emptyCompilerResult =
  CompilerResult {errors = [], compilationUnit = CompilationUnit {defns = []}}

emitDefn :: Defn -> State CompilerState ()
emitDefn defn = do
  oldResult <- gets result
  CompilationUnit {defns = oldDefns} <- gets (compilationUnit . result)
  let newResult = oldResult {compilationUnit = CompilationUnit $ oldDefns ++ [defn]}
  modify $ \state -> state {result = newResult}

emitError :: SourcePos -> String -> State CompilerState ()
emitError pos msg = do
  let err = KaleidoError pos msg
  oldResult <- gets result
  let newResult = oldResult {errors = errors oldResult ++ [err]}
  modify $ \state -> state {result = newResult}

-- mkCompilerState :: CompilerState
-- mkCompilerState funcState =
--   {
--     compilationUnit = CompilationUnit [],
--     errors = [],
--     funcState =
--   }
