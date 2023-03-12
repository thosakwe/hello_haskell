{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module WASMPass where

import Control.Monad.State
import Data.List (elemIndex)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import GHC.IO (unsafePerformIO)
import GHC.Natural
import qualified IR
import Language.Wasm as Wasm
import Language.Wasm.Structure as Wasm
import qualified Syntax as Untyped

data WASMPassState = WASMPassState
  { -- module_ :: Module,
    -- typeIndices :: Map.Map

    -- | The original IR compilation unit we are turning into WASM.
    originalIR :: IR.CompilationUnit,
    currentBlockName :: String,
    currentFunctionName :: String,
    -- | A lookup of function blocks, mapped to function names.
    -- | functionName is the index, then each entry is a map where the block
    -- | name is the index.
    functionBlocks :: FunctionBlockMap,
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
  deriving (Show)

type FunctionBlockMap = Map.Map String (Map.Map String [Wasm.Instruction Natural])

type WASMPassM = StateT WASMPassState IO

runWASMPass :: IR.CompilationUnit -> IO Module
runWASMPass unit = do
  (_, state) <- runStateT (compileUnit unit) (emptyState {originalIR = unit})
  let WASMPassState {exportList, functionBlocks, functionsMap, funcTypes, importList} = state
  -- Combine functions with their corresponding blocks before emitting WASM
  let functions = Map.toList functionsMap
  let combinedFunctions = map (combineFunctionWithBlocks functionBlocks) functions
  return
    Module
      { types = funcTypes,
        functions = combinedFunctions,
        tables = [],
        mems = [],
        globals = [],
        elems = [],
        datas = [],
        start = Nothing,
        imports = importList,
        exports = exportList
      }

-- | Inlines all blocks from the state corresponding with this function into
-- | the function.
-- |
-- | Within WASMPassState, basic blocks are kept in a separate map so that
-- | it's possible to modify them from compiler methods. But when emitting the
-- | final module, these blocks must live within a function.
combineFunctionWithBlocks :: FunctionBlockMap -> (String, Wasm.Function) -> Wasm.Function
combineFunctionWithBlocks functionBlocks (name, func) =
  case Map.lookup name functionBlocks of
    Nothing -> func
    Just blockMap ->
      let blocks = Map.elems blockMap
          blockInstrs =
            map
              ( \block ->
                  Wasm.Block
                    { blockType = Wasm.Inline Nothing,
                      body = block
                    }
              )
              blocks
          Wasm.Function {body} = func
       in func {body = body ++ blockInstrs}

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
  -- Register and export the function
  let funcType = compileFuncSig sig
  emitAndExportFunction name funcType
  -- TODO (thosakwe): Compile locals, if any...
  -- Switch context before compilation.
  switchToFunction name
  -- Compile each basic block.
  mapM_ compileBlock blocks
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
  compileInstr instr

compileBlock :: IR.BasicBlock -> WASMPassM ()
compileBlock (IR.BasicBlock {name, instrs}) = do
  -- Switch into the given block and compile the instructions.
  switchToBlock name
  mapM_ compileInstr instrs

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

-- | Compiles an IR instruction to WASM.
-- | Because WASM is a stack machine, we don't need to return a value here.
compileInstr :: IR.Instr -> WASMPassM ()
compileInstr (IR.Float value) = emitInstr $ Wasm.F64Const value
compileInstr (IR.BinOp op left right) = do
  compileInstr left
  compileInstr right
  case op of
    Untyped.Plus -> emitInstr $ Wasm.FBinOp Wasm.BS64 Wasm.FAdd
    Untyped.Minus -> emitInstr $ Wasm.FBinOp Wasm.BS64 Wasm.FSub
    Untyped.Times -> emitInstr $ Wasm.FBinOp Wasm.BS64 Wasm.FMul
    Untyped.Divide -> emitInstr $ Wasm.FBinOp Wasm.BS64 Wasm.FDiv
    Untyped.LessThan -> emitInstr $ Wasm.FRelOp Wasm.BS64 Wasm.FLt
    Untyped.GreaterThan -> emitInstr $ Wasm.FRelOp Wasm.BS64 Wasm.FGt
compileInstr (IR.GetParam name returnType) = return ()
compileInstr (IR.GetFunc name returnType) = return ()
compileInstr (IR.Call {target, args}) = do
  case target of
    -- Only a GetFunc can be called, because it includes a function name.
    IR.GetFunc name _ -> do
      -- WASM calls require the index of the function, so look it up in the
      -- functionsMap.
      funcMap <- gets functionsMap
      case elemIndex name (Map.keys funcMap) of
        Nothing -> return ()
        Just funcIndex -> do
          -- Compile each of the arguments, so they'll be present on the stack.
          mapM_ compileInstr args
          emitInstr $ Wasm.Call (fromIntegral funcIndex)
    _ -> return ()
compileInstr (IR.JumpIfTrue returnType cond thenBlockName elseBlockName) = do
  compileInstr cond
  -- We already created separate basic blocks for the then and else branches.
  -- Now, all we need to do is jump to them.
  -- The `br` instruction takes an index, so we'll need to get the index of
  -- each block within the function.
  funcName <- gets currentFunctionName
  ir <- gets originalIR
  -- Lookup the original function.
  let mOrigFunc = Map.lookup funcName (IR.defns ir)
  case mOrigFunc of
    Just (IR.FuncDefn (IR.Func {name, sig, locals, blocks})) -> do
      -- Next, get the indices of both blocks.
      let keys = Map.keys blocks
      let mThenIndex = elemIndex thenBlockName keys
      let mElseIndex = elemIndex elseBlockName keys
      case (mThenIndex, mElseIndex) of
        (Just thenIndex, Just elseIndex) -> do
          -- If both blocks exist, create inline blocks with jumps.
          let thenJumpBlock =
                Wasm.Block
                  { blockType = Wasm.Inline Nothing,
                    body = [Wasm.Br (fromIntegral thenIndex)]
                  }
          let elseJumpBlock =
                Wasm.Block
                  { blockType = Wasm.Inline Nothing,
                    body = [Wasm.Br (fromIntegral elseIndex)]
                  }
          emitInstr $
            Wasm.If
              { blockType = Wasm.Inline Nothing,
                true = [thenJumpBlock],
                false = [elseJumpBlock]
              }
        _ -> return ()
    _ -> return ()

-- TODO (thosakwe): Grab index
-- Compile both the thenBlock and elseBlock to
compileInstr IR.UnknownInstr = return ()

-- CONSTANTS
emptyState :: WASMPassState
emptyState =
  WASMPassState
    { originalIR = IR.CompilationUnit {defns = Map.empty, mainInstrs = []},
      currentBlockName = "",
      currentFunctionName = "",
      functionBlocks = Map.empty,
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
  -- Lookup basic block, don't emit raw into the function
  modifyCurrentBlock instr $ \instrs -> instrs ++ [instr]

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

-- | Helper for modifying the current block, if any exists.
modifyCurrentBlock ::
  Wasm.Instruction Natural ->
  ([Wasm.Instruction Natural] -> [Wasm.Instruction Natural]) ->
  WASMPassM ()
modifyCurrentBlock instr f = do
  WASMPassState {currentBlockName, currentFunctionName, functionBlocks} <- get
  case Map.lookup currentFunctionName functionBlocks of
    Nothing -> do
      return ()
    Just existingBlockMap -> do
      case Map.lookup currentBlockName existingBlockMap of
        Nothing -> do
          return ()
        Just instrs -> do
          -- Now that we've found the correct block/instruction list, perform
          -- the transformation.
          let newBlockMap = Map.insert currentBlockName (f instrs) existingBlockMap
          let newFuncBlockMap = Map.insert currentFunctionName newBlockMap functionBlocks
          modify $ \state -> state {functionBlocks = newFuncBlockMap}

switchToFunction :: String -> WASMPassM ()
switchToFunction funcName = do
  modify $ \state -> state {currentFunctionName = funcName}
  switchToBlock "entry"

switchToBlock :: String -> WASMPassM ()
switchToBlock blockName = do
  WASMPassState {currentFunctionName, functionBlocks} <- get
  -- If the current function doesn't exist in the map yet, create
  -- a new entry. Otherwise, get the existing one and append to it.
  -- If the block we want to switch to doesn't exist in the map yet, create
  -- a new entry. Otherwise, get the existing one and append to it.
  case Map.lookup currentFunctionName functionBlocks of
    Just existingBlockMap -> do
      case Map.lookup blockName existingBlockMap of
        Just _ -> do
          -- Just change the current block name, no need to append anything.
          modify $ \state ->
            state {currentBlockName = blockName}
        Nothing -> do
          -- Add a new basic block
          let newBlockMap = Map.insert blockName [] existingBlockMap
          let newFuncBlockMap = Map.insert currentFunctionName newBlockMap functionBlocks
          modify $ \state ->
            state
              { functionBlocks = newFuncBlockMap,
                currentBlockName = blockName
              }
    Nothing -> do
      let newBlockMap = Map.fromList [(blockName, [])]
      let newFuncBlockMap = Map.insert currentFunctionName newBlockMap functionBlocks
      modify $ \state ->
        state
          { functionBlocks = newFuncBlockMap,
            currentBlockName = blockName
          }
