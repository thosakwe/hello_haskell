module AddressBook.Prompts where

import System.IO (hFlush, stdout)

promptForName :: IO String
promptForEmail :: IO String
promptForPhone :: IO String
promptForName = do
  putStr "Enter the name: "
  hFlush stdout
  getLine

promptForEmail = do
  putStr "Enter the email address: "
  hFlush stdout
  getLine

promptForPhone = do
  putStr "Enter the phone number: "
  hFlush stdout
  getLine