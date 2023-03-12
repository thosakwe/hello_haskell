module Main where

import Control.Monad.Trans
import Parser
import System.Console.Haskeline
import TypedASTPass

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

main :: IO ()
-- runInputT is a transformer from haskeline, similar to readline
main = runInputT defaultSettings loop
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
