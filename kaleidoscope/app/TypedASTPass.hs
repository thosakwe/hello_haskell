{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A semantic analyzer that turns the untyped AST into a typed one.
module TypedASTPass where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Map as Map
import KaleidoError
import qualified Syntax as Untyped
import Text.Parsec (SourcePos)
import TypedAST
import Control.Arrow (ArrowChoice(left))

data CompilerState = CompilerState
  { funcState :: FuncState,
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
compileTopLevelExpr (Untyped.Function pos name params body) = do
  params <- mapM compileParam params
  let sig = FuncSignature {params = Map.fromList params, returnType = FloatType}
  let locals = Map.empty
  -- Create the "entry" block.
  let entryBlock = BasicBlock "entry" []
  let blocks = Map.fromList [("entry", entryBlock)]
  -- Emit the function definition, before we compile the body.
  let func = Func {name, sig, locals, blocks}
  emitDefn name $ FuncDefn func
  -- Now that we have the function defined, create a func state, and compile
  -- the body.
  let funcState = FuncState {currentBlockName = "entry", currentFuncName = name}
  modify $ \state -> state {funcState}
  compileFuncBody body
compileTopLevelExpr (Untyped.Extern pos name params) = do
  params <- mapM compileParam params
  let sig =
        FuncSignature
          { returnType = FloatType,
            params = Map.fromList params
          }
  emitDefn name $ ExternDefn name sig
compileTopLevelExpr expr = do
  let pos = Untyped.getPos expr
  emitError pos "Not a function or extern."

compileParam :: Untyped.Expr -> State CompilerState (String, Type)
compileParam (Untyped.Var _ name) = return (name, FloatType)
compileParam expr = do
  let pos = Untyped.getPos expr
  emitError pos "Not a var."
  return ("?", UnknownType)

compileFuncBody :: Untyped.Expr -> State CompilerState ()
compileFuncBody body = do
  instr <- compileExpr body
  let pos = Untyped.getPos body
  emitInstr pos instr

compileExpr :: Untyped.Expr -> State CompilerState Instr
compileExpr (Untyped.Float pos value) = do
  return $ Float value
compileExpr (Untyped.BinOp pos op left right) = do
  left <- compileExpr left
  right <- compileExpr right
  return $ BinOp op left right
compileExpr (Untyped.Var pos name) = do
  -- Try to lookup the given param...
  mParamType <- lookupParam name
  case mParamType of
    Nothing -> do
      emitError pos $ "No such param '" ++ name ++ "'."
      return UnknownInstr
    Just paramType -> do
      return $ GetParam name paramType
compileExpr (Untyped.Call pos funcName args) = do
  mFunc <- lookupFunc funcName
  case mFunc of
    Nothing -> do
      return UnknownInstr
    Just (Func {name, sig, locals, blocks}) -> do
      let funcType = FuncType sig
      let target = GetFunc name funcType
      typedArgs <- mapM compileExpr args
      return $ Call {target, args = typedArgs}
compileExpr expr = do
  let msg = "Unsupported expr within function: " ++ show expr
  let pos = Untyped.getPos expr
  emitError pos msg
  return UnknownInstr

emptyCompilerState :: CompilerState
emptyCompilerState =
  let emptyFuncState = FuncState {currentFuncName = "", currentBlockName = ""}
   in CompilerState {funcState = emptyFuncState, result = emptyCompilerResult}

emptyCompilerResult :: CompilerResult
emptyCompilerResult =
  CompilerResult
    { errors = [],
      compilationUnit = CompilationUnit {defns = Map.empty}
    }

emitDefn :: String -> Defn -> State CompilerState ()
emitDefn name defn = do
  modifyCompilationUnit $ \unit ->
    let CompilationUnit {defns = oldDefns} = unit
     in unit {defns = Map.insert name defn oldDefns}

emitInstr :: SourcePos -> Instr -> State CompilerState ()
emitInstr pos instr = do
  -- Find the func with the current func name.
  FuncState {currentBlockName, currentFuncName} <- gets funcState
  mfunc <- lookupCurrentFunc
  case mfunc of
    Nothing ->
      emitError pos $ "No function named '" ++ currentFuncName ++ "' exists."
    Just func -> do
      -- Find the basic block.
      let mBlock = Map.lookup currentBlockName $ blocks func
      case mBlock of
        Nothing ->
          let msg =
                "No block named '"
                  ++ currentBlockName
                  ++ "' exists in this function."
           in emitError pos msg
        Just block -> do
          -- Add the instr.
          let newInstrs = instrs block ++ [instr]
          let newBlock = block {instrs = newInstrs}
          let newBlocks = Map.insert currentBlockName newBlock (blocks func)
          let newFunc = FuncDefn $ func {blocks = newBlocks}
          -- Create a new definitions map.
          -- Edit the compilation unit
          modifyCompilationUnit $ \unit ->
            unit
              { defns = Map.insert currentFuncName newFunc (defns unit)
              }

emitError :: SourcePos -> String -> State CompilerState ()
emitError pos msg = do
  let err = KaleidoError pos msg
  modifyResult $ \result -> result {errors = errors result ++ [err]}

lookupCurrentFunc :: State CompilerState (Maybe Func)
lookupCurrentFunc = do
  FuncState {currentBlockName, currentFuncName} <- gets funcState
  lookupFunc currentFuncName

lookupFunc :: String -> State CompilerState (Maybe Func)
lookupFunc name = do
  unit <- getCompilationUnit
  let mFunc = Map.lookup name (defns unit)
  case mFunc of
    Just (FuncDefn func) -> return $ Just func
    _ -> return Nothing

lookupParam :: String -> State CompilerState (Maybe Type)
lookupParam name = do
  mFunc <- lookupCurrentFunc
  case mFunc of
    Nothing -> return Nothing
    Just func -> do
      let FuncSignature {returnType, params} = sig func
      return $ Map.lookup name params

getResult :: State CompilerState CompilerResult
getResult = gets result

getCompilationUnit :: State CompilerState CompilationUnit
getCompilationUnit = gets (compilationUnit . result)

modifyResult :: (CompilerResult -> CompilerResult) -> State CompilerState ()
modifyResult f = do
  newResult <- gets (f . result)
  modify $ \state -> state {result = newResult}

modifyCompilationUnit :: (CompilationUnit -> CompilationUnit) -> State CompilerState ()
modifyCompilationUnit f =
  modifyResult $ \result -> result {compilationUnit = f (compilationUnit result)}

-- mkCompilerState :: CompilerState
-- mkCompilerState funcState =
--   {
--     compilationUnit = CompilationUnit [],
--     errors = [],
--     funcState =
--   }
