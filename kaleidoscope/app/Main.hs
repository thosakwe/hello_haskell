module Main where

import Control.Monad.Trans
import Parser
import System.Console.Haskeline
import System.Environment (getArgs)
import TypedASTPass
import TypedAST (ppCompilationUnit)
import Text.PrettyPrint

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
      let result = runTypedASTPass exprs
      case errors result of
        [] -> do
          putStrLn "Typed compilation unit:"
          -- print $ compilationUnit result
          putStrLn $ render $ ppCompilationUnit $ compilationUnit result
        errors -> do
          putStrLn $ "Total errors: " ++ show (length errors)
          mapM_ print errors
