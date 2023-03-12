{-# LANGUAGE NamedFieldPuns #-}

-- | A semantic analyzer that turns the untyped AST into a typed one.
module TypedASTPass where

import Control.Monad.State
import KaleidoError
import qualified Syntax as Untyped
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

emptyCompilerState :: CompilerState
emptyCompilerState =
  CompilerState {funcState = Nothing, result = emptyCompilerResult}

emptyCompilerResult :: CompilerResult
emptyCompilerResult =
  CompilerResult {errors = [], compilationUnit = CompilationUnit {defns = []}}

-- mkCompilerState :: CompilerState
-- mkCompilerState funcState =
--   {
--     compilationUnit = CompilationUnit [],
--     errors = [],
--     funcState =
--   }

analyzeTopLevelExpr :: Untyped.Expr -> ()
analyzeTopLevelExpr _ = ()

compileTopLevel :: [Untyped.Expr] -> State CompilerState ()
compileTopLevel untypedExprs = do
  return ()

runTypedASTPass :: Untyped.CompilationUnit -> CompilerResult
runTypedASTPass untypedExprs =
  -- let _ = map analyzeTopLevelExpr untypedExprs in
  -- let _ = runState analyzeTopLevelExpr untypedExprs in
  let (_, CompilerState {result}) = runState (compileTopLevel untypedExprs) emptyCompilerState
   in CompilerResult
        { compilationUnit = compilationUnit result,
          errors = errors result
        }
