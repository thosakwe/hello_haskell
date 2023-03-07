module AddressBook.Types
  ( Contact (Contact, email, name, phone),
    ContactList,
    ContactAppState,
    defaultContact,
  )
where

import Control.Monad.State (StateT)

data Contact = Contact {name :: String, email :: String, phone :: String}
  deriving (Show)

type ContactList = [Contact]

type ContactAppState = StateT ContactList IO

defaultContact :: Contact
defaultContact =
  Contact
    { name = "Empire Today",
      email = "info@empiretoday.com",
      phone = "800-588-2300"
    }
