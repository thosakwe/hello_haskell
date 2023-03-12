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
    exportList :: [Wasm.Export]
  }

type WasmPassM = StateT WASMPassState IO

runWASMPass :: IR.CompilationUnit -> IO Module
runWASMPass unit = do
  (_, state) <- runStateT (compileUnit unit) emptyState
  let WASMPassState {functionsMap, funcTypes} = state
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
        imports = [],
        exports = []
      }

compileUnit :: IR.CompilationUnit -> WasmPassM ()
compileUnit unit = do
  -- Register and export the "main" function
  let mainType = Wasm.FuncType {params = [], results = [Wasm.F64]}
  mainIndex <- emitFunction "main" mainType
  emitExport $
    Wasm.Export
      { name = TL.pack "main",
        desc = Wasm.ExportFunc (fromIntegral mainIndex)
      }
  return ()

compileFuncSig :: IR.FuncSignature -> Wasm.FuncType
compileFuncSig (IR.FuncSignature {returnType, params}) =
  let wasmParams = map compileType $ Map.elems params
      wasmReturnType = compileType returnType
   in Wasm.FuncType {params = wasmParams, results = [wasmReturnType]}

compileType :: IR.Type -> Wasm.ValueType
compileType IR.FloatType = Wasm.F64
compileType (IR.UnknownType) = Wasm.F64
-- TODO (thosakwe): Is this the right type to compile a function pointer to...?
compileType (IR.FuncType sig) = Wasm.F64

emptyState :: WASMPassState
emptyState =
  WASMPassState
    { currentFunctionName = "",
      functionsMap = Map.empty,
      funcTypes = [],
      exportList = []
    }

emitExport :: Wasm.Export -> WasmPassM ()
emitExport export =
  modify $ \state -> state {exportList = exportList state ++ [export]}

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
