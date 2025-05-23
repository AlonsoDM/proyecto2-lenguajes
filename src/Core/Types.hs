{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Core.Types where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)


---------------------------------
-- Main Types (User, Password) --
---------------------------------


data User = User                                            -- User data type
  { username :: T.Text                                      -- Username (unique identifier)
  , pinHash :: BS.ByteString                                -- Hashed PIN (for authentication)
  , salt :: BS.ByteString                                   -- Salt for hashing and encryption
  , masterKey :: BS.ByteString                              -- Encrypted master key (decrypted with PIN)
  } deriving (Show, Eq)

data PasswordEntry = PasswordEntry                          -- Password entry data type
  { entryId :: T.Text                                       -- Unique ID for the entry
  , serviceName :: T.Text                                   -- Name of the service
  , accountName :: T.Text                                   -- Account name or email
  , encryptedPassword :: T.Text                             -- Encrypted password
  , createdAt :: UTCTime                                    -- Creation timestamp
  , updatedAt :: UTCTime                                    -- Last update timestamp
  } deriving (Show, Eq, Generic)



instance ToJSON PasswordEntry                               -- JSON serialization instance for PasswordEntry
instance FromJSON PasswordEntry                             -- JSON deserialization instance for PasswordEntry

data Vault = Vault                                          -- Vault containing all password entries for a user
  { owner :: T.Text                                         -- Username of the vault owner
  , entries :: [PasswordEntry]                              -- List of password entries
  } deriving (Show, Eq, Generic)



instance ToJSON Vault                                       -- JSON serialization/deserialization instances for Vault
instance FromJSON Vault

data Result a                                               -- A generic result type for application operations
  = Success a                                               -- Successful result containing a value of type 'a'
  | Error T.Text                                            
  deriving (Show, Eq, Functor)

data Session = Session                                      -- Session data for authenticated user
  { sessionUser :: User
  , sessionKey :: BS.ByteString                             -- Decrypted master key for encryption/decryption
  } deriving (Show, Eq)