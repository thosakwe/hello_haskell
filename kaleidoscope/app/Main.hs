module Main where

import Control.Monad.Trans
import Parser
import System.Console.Haskeline
import TypedASTPass
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Args: "
  mapM_ print args
  case args of
    [] -> runREPL
    (fname:_) -> do
      contents <- readFile fname
      process contents

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
          liftIO $ process input
          loop

process :: String -> IO ()
process line = do
  let result = parseToplevel line
  case result of
    Left err -> print err
    Right exprs -> do
      putStrLn $ "Total exprs: " ++ show (length exprs)
      mapM_ print exprs
      let result = runTypedASTPass exprs
      putStrLn $ "Total errors: " ++ show (length $ errors result)
      mapM_ print $ errors result
      putStrLn "Typed compilation unit:"
      print $ compilationUnit result
