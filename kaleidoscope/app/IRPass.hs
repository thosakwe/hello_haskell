{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A semantic analyzer that turns the untyped AST into a typed one.
module IRPass where

import Control.Arrow (ArrowChoice (left))
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)
import IR
import KaleidoError
import qualified Syntax as Untyped
import Text.Parsec (SourcePos)

data CompilerState = CompilerState
  { funcState :: FuncState,
    result :: CompilerResult,
    names :: Set.Set String
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

type CompilerM = State CompilerState

runIRPass :: Untyped.CompilationUnit -> CompilerResult
runIRPass untypedExprs =
  -- let _ = map analyzeTopLevelExpr untypedExprs in
  -- let _ = runState analyzeTopLevelExpr untypedExprs in
  let (_, CompilerState {result}) = runState (compileUnit untypedExprs) emptyCompilerState
   in CompilerResult
        { compilationUnit = compilationUnit result,
          errors = errors result
        }

compileUnit :: [Untyped.Expr] -> CompilerM ()
compileUnit untypedExprs = do
  mapM_ compileTopLevelExpr untypedExprs

compileTopLevelExpr :: Untyped.Expr -> CompilerM ()
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
  instr <- compileExpr expr
  emitMainInstr instr

compileParam :: Untyped.Expr -> CompilerM (String, Type)
compileParam (Untyped.Var _ name) = return (name, FloatType)
compileParam expr = do
  let pos = Untyped.getPos expr
  emitError pos "Not a var."
  return ("?", UnknownType)

compileFuncBody :: Untyped.Expr -> CompilerM ()
compileFuncBody body = do
  instr <- compileExpr body
  let pos = Untyped.getPos body
  emitInstr pos instr

compileExpr :: Untyped.Expr -> CompilerM Instr
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
compileExpr (Untyped.If pos cond then_ else_) = do
  cond <- compileExpr cond
  -- TODO (thosakwe): If we ever return types besides float, then we need
  -- to figure out the return type
  -- TODO (thosakwe): Add a unique-name fetcher
  -- We need to create 2 new basic blocks, one for if true, one for if false.
  thenBlockName <- getUniqueName "then"
  elseBlockName <- getUniqueName "else"
  emitNewBlock thenBlockName
  emitNewBlock elseBlockName
  -- To compile the logic for if and else, we need to create a new FuncState
  -- for each.
  thenBlockState <- gets $ changeBlock thenBlockName
  -- Preserve the current basic block name, so we can return to it after
  -- compiling both branches.
  oldFuncState <- gets funcState
  let compileBranches = runState $ do
        compileFuncBody then_
        modify $ changeBlock elseBlockName
        compileFuncBody else_
  let (_, finalState) = compileBranches thenBlockState
  put finalState {funcState = oldFuncState}
  return $ JumpIfTrue FloatType cond thenBlockName elseBlockName
compileExpr expr = do
  let msg = "Unsupported expr within function: " ++ show expr
  let pos = Untyped.getPos expr
  emitError pos msg
  return UnknownInstr

emptyCompilerState :: CompilerState
emptyCompilerState =
  let emptyFuncState = FuncState {currentFuncName = "", currentBlockName = ""}
   in CompilerState
        { funcState = emptyFuncState,
          names = Set.empty,
          result = emptyCompilerResult
        }

emptyCompilerResult :: CompilerResult
emptyCompilerResult =
  CompilerResult
    { errors = [],
      compilationUnit = CompilationUnit {defns = Map.empty, mainInstrs = []}
    }

emitDefn :: String -> Defn -> CompilerM ()
emitDefn name defn = do
  modifyCompilationUnit $ \unit ->
    let CompilationUnit {defns = oldDefns} = unit
     in unit {defns = Map.insert name defn oldDefns}

emitMainInstr :: Instr -> CompilerM ()
emitMainInstr instr =
  modifyCompilationUnit $ \unit ->
    unit {mainInstrs = mainInstrs unit ++ [instr]}

-- | Emit a new instruction in the current basic block.
emitInstr :: SourcePos -> Instr -> CompilerM ()
emitInstr pos instr = do
  FuncState {currentBlockName, currentFuncName} <- gets funcState
  modifyCurrentFunction $ \func -> do
    -- Find the basic block.
    let mBlock = Map.lookup currentBlockName $ blocks func
    case mBlock of
      Nothing -> func
      Just block -> do
        -- Add the instr.
        let newInstrs = instrs block ++ [instr]
        let newBlock = block {instrs = newInstrs}
        let newBlocks = Map.insert currentBlockName newBlock (blocks func)
        func {blocks = newBlocks}

-- | Create a new block in the current function.
emitNewBlock :: String -> CompilerM ()
emitNewBlock name = do
  modifyCurrentFunction $ \func ->
    let newBlock = BasicBlock {name, instrs = []}
        newBlocks = Map.insert name newBlock (blocks func)
     in func {blocks = newBlocks}

emitError :: SourcePos -> String -> CompilerM ()
emitError pos msg = do
  let err = KaleidoError pos msg
  modifyResult $ \result -> result {errors = errors result ++ [err]}

lookupCurrentFunc :: CompilerM (Maybe Func)
lookupCurrentFunc = do
  FuncState {currentBlockName, currentFuncName} <- gets funcState
  lookupFunc currentFuncName

lookupFunc :: String -> CompilerM (Maybe Func)
lookupFunc name = do
  unit <- getCompilationUnit
  let mFunc = Map.lookup name (defns unit)
  case mFunc of
    Just (FuncDefn func) -> return $ Just func
    _ -> return Nothing

lookupParam :: String -> CompilerM (Maybe Type)
lookupParam name = do
  mFunc <- lookupCurrentFunc
  case mFunc of
    Nothing -> return Nothing
    Just func -> do
      let FuncSignature {returnType, params} = sig func
      return $ Map.lookup name params

getResult :: CompilerM CompilerResult
getResult = gets result

getCompilationUnit :: CompilerM CompilationUnit
getCompilationUnit = gets (compilationUnit . result)

getUniqueName :: String -> CompilerM String
getUniqueName name = getNameWithIndex name 0

getNameWithIndex :: String -> Int -> CompilerM String
getNameWithIndex name idx = do
  let nameWithIndex = name ++ show idx
  names <- gets names
  if Set.member nameWithIndex names
    then getNameWithIndex name (idx + 1)
    else do
      let newNames = Set.insert nameWithIndex names
      modify $ \state -> state {names = newNames}
      return nameWithIndex

--  in if not Set.member nameWithIndex names
--       then return "a"
--       else return "b"

modifyResult :: (CompilerResult -> CompilerResult) -> CompilerM ()
modifyResult f = do
  newResult <- gets (f . result)
  modify $ \state -> state {result = newResult}

modifyCompilationUnit :: (CompilationUnit -> CompilationUnit) -> CompilerM ()
modifyCompilationUnit f =
  modifyResult $ \result -> result {compilationUnit = f (compilationUnit result)}

modifyCurrentFunction :: (Func -> Func) -> CompilerM ()
modifyCurrentFunction f = do
  mFunc <- lookupCurrentFunc
  case mFunc of
    Nothing -> return ()
    Just func ->
      modifyCompilationUnit $ \unit ->
        let Func {name = currentFuncName} = func
            newFunc = FuncDefn $ f func
         in -- Create a new definitions map.
            -- Edit the compilation unit
            unit
              { defns = Map.insert currentFuncName newFunc (defns unit)
              }

-- mkCompilerState :: CompilerState
-- mkCompilerState funcState =
--   {
--     compilationUnit = CompilationUnit [],
--     errors = [],
--     funcState =
--   }

-- | Copies the compiler state, but with a different basic block as the
-- context.
changeBlock :: String -> CompilerState -> CompilerState
changeBlock newBlockName state =
  let fs = funcState state
      newFuncState = fs {currentBlockName = newBlockName}
   in state {funcState = newFuncState}