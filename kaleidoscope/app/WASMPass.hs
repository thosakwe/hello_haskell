{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module WASMPass where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
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

type WasmPassM = StateT WASMPassState IO

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

compileUnit :: IR.CompilationUnit -> WasmPassM ()
compileUnit unit = do
  -- Register and export the "main" function
  let mainType = Wasm.FuncType {params = [], results = [Wasm.F64]}
  emitAndExportFunction "main" mainType
  -- Compile each definition
  mapM_ compileDefn $ IR.defns unit

compileDefn :: IR.Defn -> WasmPassM ()
compileDefn (IR.MainInstr instr) = return ()
compileDefn (IR.FuncDefn func) = return ()
compileDefn (IR.ExternDefn name sig) = do
  let funcType = compileFuncSig sig
  funcTypeIndex <- emitFuncType funcType
  emitImport $
    Wasm.Import
      { sourceModule = TL.pack importModuleName,
        name = TL.pack name,
        desc = Wasm.ImportFunc (fromIntegral funcTypeIndex)
      }

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

emitExport :: Wasm.Export -> WasmPassM ()
emitExport export =
  modify $ \state -> state {exportList = exportList state ++ [export]}

emitImport :: Wasm.Import -> WasmPassM ()
emitImport import_ =
  modify $ \state -> state {importList = importList state ++ [import_]}

-- | Emit a new function, and add it to the module's exports.
emitAndExportFunction :: String -> FuncType -> WasmPassM ()
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
emitFunction :: String -> FuncType -> WasmPassM Int
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
emitFuncType :: FuncType -> WasmPassM Int
emitFuncType type_ = do
  funcTypes <- gets funcTypes
  let funcTypeIndex = fromIntegral $ length funcTypes
  modify $ \state ->
    state {funcTypes = funcTypes ++ [type_]}
  return funcTypeIndex