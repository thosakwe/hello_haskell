module AddressBook.Store where

import AddressBook.Types
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), State, StateT, modify, runStateT)

createContactsTable :: IO ()
deleteContactByName :: String -> ContactAppState ()
insertNewContact :: Contact -> ContactAppState ()
lookupContactByName :: String -> ContactAppState (Maybe Contact)
retrieveAllContacts :: ContactAppState ContactList
createContactsTable = return ()

deleteContactByName contactName = do
  -- Because Haskell can do partial function application, we only need to
  -- pass the first argument (a lambda) to `filter`.
  modify $ filter (\contact -> name contact /= contactName)

lookupContactByName contactName = do
  contacts <- retrieveAllContacts
  let matchingContacts =
        filter (\contact -> name contact == contactName) contacts
  case matchingContacts of
    [] -> return Nothing
    (contact : _) -> return $ Just contact

insertNewContact contact =
  -- Again, this works because of function application. We can use the ++
  -- operator as a function on its own.
  modify (++ [contact])

retrieveAllContacts = get