module Main where

factorial :: Int -> Int
factorial n = product [1..n]

loop :: IO ()
loop = do
    putStrLn "Enter a number, or say 'quit':"
    line <- getLine
    if line == "quit" then
      return ()
    else do
      let n = read line :: Int
      print $ factorial n 
      loop

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  loop
