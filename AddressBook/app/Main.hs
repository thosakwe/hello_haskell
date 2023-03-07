-- A simple state-driven contact list manager.
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import AddressBook.Prompts
  ( promptForEmail,
    promptForName,
    promptForPhone,
  )
import AddressBook.Store
  ( createContactsTable,
    deleteContactByName,
    insertNewContact,
    lookupContactByName,
    retrieveAllContacts,
  )
import AddressBook.Types
  ( Contact (Contact, email, name, phone),
    ContactAppState,
    defaultContact,
  )
import Control.Exception (handle)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), State, StateT, modify, runStateT)
import System.IO (hFlush, stdout)

main :: IO ()
loop :: ContactAppState ()
handleChoice :: Int -> ContactAppState ()
main = do
  createContactsTable
  putStrLn "Welcome to Regios Contacts."
  (result, finalContactList) <- runStateT loop []
  putStrLn $ "Final contact list: " ++ show finalContactList
  return result

loop = do
  input <- liftIO $ do
    putStrLn "What would you like to do?"
    putStrLn "1.) Retrieve all contacts"
    putStrLn "2.) Lookup a contact by name"
    putStrLn "3.) Insert a new contact"
    putStrLn "4.) Delete a contact"
    putStrLn "5.) Insert default contact into the list."
    putStrLn "6.) Quit"
    putStr "Enter a number [1-6]: "
    -- hFlush is necessary to print the above line without waiting for the user
    -- to press Enter.
    hFlush stdout
    getLine
  let choice = read input :: Int
  handleChoice choice

handleChoice choice =
  case choice of
    -- Retrieve all contacts
    1 -> do
      contacts <- retrieveAllContacts
      liftIO $ print contacts
      loop
    -- Lookup a contact by name
    2 -> do
      name <- liftIO promptForName
      contactMaybe <- lookupContactByName name
      liftIO $ do
        case contactMaybe of
          Just contact -> print contact
          Nothing ->
            putStrLn $ "No contact exists with the name " ++ name ++ "."
      loop
    -- Insert a new contact
    3 -> do
      contact <- liftIO $ do
        name <- promptForName
        email <- promptForEmail
        phone <- promptForPhone
        return Contact {name, email, phone}
      insertNewContact contact
      liftIO $ putStrLn $ "Inserted: " ++ show contact
      loop
    -- Delete a contact
    4 -> do
      name <- liftIO promptForName
      deleteContactByName name
      liftIO $ putStrLn $ "Deleted all contacts named " ++ name ++ "."
      loop
    -- Insert default contact
    5 -> do
      insertNewContact defaultContact
      liftIO $ putStrLn $ "Inserted: " ++ show defaultContact
      loop
    -- Quit
    6 -> do
      liftIO $ putStrLn "Have a lovely day!"
    _ -> do
      liftIO $ putStrLn "Invalid option. You must enter a number 1-6."
      loop
