{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module WASMPass where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import GHC.IO (unsafePerformIO)
import GHC.Natural
import qualified IR
import Language.Wasm as Wasm
import Language.Wasm.Structure as Wasm

data WASMPassState = WASMPassState
  { -- module_ :: Module,
    -- typeIndices :: Map.Map
    currentFunctionName :: String,
    -- | A lookup table of functions that will be included in the output
    -- | module.
    functionsMap :: Map.Map String Function,
    -- | WASM functions don't have their type stored directly with them. Instead,
    -- | there is a global array of function types, and functions types are
    -- | actually just an index of this collection.
    funcTypes :: [FuncType],
    exportList :: [Wasm.Export],
    importList :: [Wasm.Import]
  }

type WASMPassM = StateT WASMPassState IO

runWASMPass :: IR.CompilationUnit -> IO Module
runWASMPass unit = do
  (_, state) <- runStateT (compileUnit unit) emptyState
  let WASMPassState {exportList, functionsMap, funcTypes, importList} = state
  return
    Module
      { types = funcTypes,
        functions = Map.elems functionsMap,
        tables = [],
        mems = [],
        globals = [],
        elems = [],
        datas = [],
        start = Nothing,
        imports = importList,
        exports = exportList
      }

compileUnit :: IR.CompilationUnit -> WASMPassM ()
compileUnit unit = do
  -- Register and export the "main" function
  let mainType = Wasm.FuncType {params = [], results = [Wasm.F64]}
  emitAndExportFunction "main" mainType
  -- Compile each definition
  mapM_ compileDefn $ IR.defns unit
  -- Compile each main instr
  mapM_ compileMainInstr $ IR.mainInstrs unit

compileDefn :: IR.Defn -> WASMPassM ()
compileDefn (IR.FuncDefn func) = do
  let IR.Func {name, sig, locals, blocks} = func
  let funcType = compileFuncSig sig
  emitAndExportFunction name funcType
compileDefn (IR.ExternDefn name sig) = do
  let funcType = compileFuncSig sig
  funcTypeIndex <- emitFuncType funcType
  emitImport $
    Wasm.Import
      { sourceModule = TL.pack importModuleName,
        name = TL.pack name,
        desc = Wasm.ImportFunc (fromIntegral funcTypeIndex)
      }

compileMainInstr :: IR.Instr -> WASMPassM ()
compileMainInstr instr = do
  -- Compile this instruction into the "main" function
  switchToFunction "main"
  wasmInstr <- compileInstr instr
  emitInstr wasmInstr

compileFuncSig :: IR.FuncSignature -> Wasm.FuncType
compileFuncSig (IR.FuncSignature {returnType, params}) =
  let wasmParams = map compileType $ Map.elems params
      wasmReturnType = compileType returnType
   in Wasm.FuncType {params = wasmParams, results = [wasmReturnType]}

compileType :: IR.Type -> Wasm.ValueType
compileType IR.FloatType = Wasm.F64
compileType IR.UnknownType = Wasm.F64
-- TODO (thosakwe): Is this the right type to compile a function pointer to...?
compileType (IR.FuncType sig) = Wasm.F64

compileInstr :: IR.Instr -> WASMPassM (Wasm.Instruction Natural)
compileInstr (IR.Float value) = return $ Wasm.F64Const value
compileInstr (IR.BinOp op left right) = return Wasm.Nop
compileInstr (IR.GetParam name returnType) = return Wasm.Nop
compileInstr (IR.GetFunc name returnType) = return Wasm.Nop
compileInstr (IR.Call {target, args}) = return Wasm.Nop
compileInstr (IR.JumpIfTrue returnType cond thenBlock elseBlock) = return Wasm.Nop
compileInstr IR.UnknownInstr = return Wasm.Nop

-- CONSTANTS
emptyState :: WASMPassState
emptyState =
  WASMPassState
    { currentFunctionName = "",
      functionsMap = Map.empty,
      funcTypes = [],
      exportList = [],
      importList = []
    }

importModuleName :: String
importModuleName = "imports"

emitExport :: Wasm.Export -> WASMPassM ()
emitExport export =
  modify $ \state -> state {exportList = exportList state ++ [export]}

emitImport :: Wasm.Import -> WASMPassM ()
emitImport import_ =
  modify $ \state -> state {importList = importList state ++ [import_]}

-- | Emit a new function, and add it to the module's exports.
emitAndExportFunction :: String -> FuncType -> WASMPassM ()
emitAndExportFunction name type_ = do
  index <- emitFunction name type_
  emitExport $
    Wasm.Export
      { name = TL.pack name,
        desc = Wasm.ExportFunc (fromIntegral index)
      }

-- | Adds a new function with the given name and type to the module.
-- | The function locals and body will be empty.
-- |
-- | Returns the index of the function.
emitFunction :: String -> FuncType -> WASMPassM Int
emitFunction name type_ = do
  funcTypes <- gets funcTypes
  functionsMap <- gets functionsMap
  -- Determine an index to insert the function's type into the module. Just use
  -- the current length of the list.
  let funcTypeIndex = fromIntegral $ length funcTypes
  let funcIndex = Map.size functionsMap
  let newFunction = Wasm.Function {funcType = funcTypeIndex, localTypes = [], body = []}
  modify $ \state ->
    state
      { functionsMap = Map.insert name newFunction functionsMap,
        funcTypes = funcTypes ++ [type_]
      }
  return funcIndex

-- | Adds a new function type to the list, and returns the index.
emitFuncType :: FuncType -> WASMPassM Int
emitFuncType type_ = do
  funcTypes <- gets funcTypes
  let funcTypeIndex = fromIntegral $ length funcTypes
  modify $ \state ->
    state {funcTypes = funcTypes ++ [type_]}
  return funcTypeIndex

emitInstr :: Instruction Natural -> WASMPassM ()
emitInstr instr = do
  -- TODO (thosakwe): Lookup basic block, don't emit raw into the function
  modifyCurrentFunction $ \func ->
    let Wasm.Function {body = oldBody} = func
     in func {body = oldBody ++ [instr]}

-- | Helper for modifying the current function, if any exists.
modifyCurrentFunction :: (Wasm.Function -> Wasm.Function) -> WASMPassM ()
modifyCurrentFunction f = do
  -- Find the current function
  WASMPassState {currentFunctionName, functionsMap} <- get
  case Map.lookup currentFunctionName functionsMap of
    Nothing -> return ()
    Just func -> do
      let newFunc = f func
      let newFunctionsMap = Map.insert currentFunctionName newFunc functionsMap
      modify $ \state -> state {functionsMap = newFunctionsMap}

switchToFunction :: String -> WASMPassM ()
switchToFunction funcName =
  modify $ \state -> state {currentFunctionName = funcName}
