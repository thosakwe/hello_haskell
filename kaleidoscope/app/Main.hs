module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy as LBS
import IR (ppCompilationUnit)
import IRPass
import qualified Language.Wasm as Wasm
import Parser
import System.Console.Haskeline
import System.Environment (getArgs)
import Text.PrettyPrint
import WASMPass

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Args: "
  mapM_ print args
  case args of
    [] -> runREPL
    (fname : _) -> do
      contents <- readFile fname
      process fname contents

runREPL :: IO ()
-- runInputT is a transformer from haskeline, similar to readline
runREPL = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "kaleido> "
      case minput of
        Nothing -> do
          -- Prints on CTRL-D
          outputStrLn "Goodbye."
        Just input -> do
          liftIO $ process "<stdin>" input
          loop

process :: String -> String -> IO ()
process fname line = do
  let result = parseCompilationUnit fname line
  case result of
    Left err -> print err
    Right exprs -> do
      -- putStrLn $ "Total exprs: " ++ show (length exprs)
      -- mapM_ print exprs
      let result = runIRPass exprs
      case errors result of
        [] -> do
          putStrLn "Typed compilation unit:"
          -- print $ compilationUnit result
          let unit = compilationUnit result
          putStrLn $ render $ ppCompilationUnit unit
          module_ <- runWASMPass unit
          print module_
          let bs = Wasm.encodeLazy module_
          LBS.writeFile "out.wasm" bs
          putStrLn "Wrote out.wasm"
        errors -> do
          putStrLn $ "Total errors: " ++ show (length errors)
          mapM_ print errors
