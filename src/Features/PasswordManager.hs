{-# LANGUAGE OverloadedStrings #-}

module Features.PasswordManager
  ( addPassword
  , getPassword
  , updatePassword
  , deletePassword
  , listPasswords
  , generatePassword
  ) where

import Core.Types
import Core.Crypto
import Persistence.Storage

-- import qualified Data.ByteString as BS
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.List (find)
import System.Random (randomRIO)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
-- import Data.Either (fromRight)
import Control.Monad (replicateM)


-------------------------
-- Password management --
-------------------------


addPassword :: Session -> T.Text -> T.Text -> T.Text -> IO (Result PasswordEntry)
addPassword session serviceName accountName password = do
  encryptedPwd <- encryptWithKey (sessionKey session) password
  
  entryId <- T.pack . UUID.toString <$> UUID.nextRandom
  
  now <- getCurrentTime
  
  let newEntry = PasswordEntry
        { entryId = entryId
        , serviceName = serviceName
        , accountName = accountName
        , encryptedPassword = encryptedPwd
        , createdAt = now
        , updatedAt = now
        }
  
  let username = Core.Types.username $ sessionUser session
  vaultResult <- loadVault username
  case vaultResult of
    Error err -> return $ Error $ T.append (T.pack "Failed to load vault: ") err
    Success vault -> do
      let updatedVault = vault { entries = newEntry : entries vault }
      
      result <- saveVault updatedVault
      case result of
        Error err -> return $ Error $ T.append (T.pack "Failed to save vault: ") err
        Success _ -> return $ Success newEntry

getPassword :: Session -> T.Text -> IO (Result (PasswordEntry, T.Text))
getPassword session entryId = do
  let username = Core.Types.username $ sessionUser session
  vaultResult <- loadVault username
  case vaultResult of
    Error err -> return $ Error $ T.append (T.pack "Failed to load vault: ") err
    Success vault -> do
      case find (\e -> entryId == Core.Types.entryId e) (entries vault) of
        Nothing -> return $ Error $ T.pack "Password entry not found"
        Just entry -> do
          case decryptWithKey (sessionKey session) (encryptedPassword entry) of
            Left _ -> return $ Error $ T.pack "Failed to decrypt password"
            Right decryptedPwd -> return $ Success (entry, decryptedPwd)

-- Modified function signature to match implementation
updatePassword :: Session -> T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Result PasswordEntry)
updatePassword session entryId mServiceName mAccountName mPassword = do
  let username = Core.Types.username $ sessionUser session
  vaultResult <- loadVault username
  case vaultResult of
    Error err -> return $ Error $ T.append (T.pack "Failed to load vault: ") err
    Success vault -> do
      case find (\e -> entryId == Core.Types.entryId e) (entries vault) of
        Nothing -> return $ Error $ T.pack "Password entry not found"
        Just entry -> do
          updatedEntry <- updateEntry entry
          
          now <- getCurrentTime
          
          let finalEntry = updatedEntry { updatedAt = now }
          
          let updatedEntries = map (\e -> if Core.Types.entryId e == entryId then finalEntry else e) (entries vault)
              updatedVault = vault { entries = updatedEntries }
          
          result <- saveVault updatedVault
          case result of
            Error err -> return $ Error $ T.append (T.pack "Failed to save vault: ") err
            Success _ -> return $ Success finalEntry
  where
    updateEntry entry = do
      newEncryptedPwd <- case mPassword of
        Just newPwd -> encryptWithKey (sessionKey session) newPwd
        Nothing -> return $ encryptedPassword entry
      
      let newServiceName = maybe (serviceName entry) (\s -> if T.null s then serviceName entry else s) mServiceName
          newAccountName = maybe (accountName entry) (\a -> if T.null a then accountName entry else a) mAccountName
      
      return $ entry
        { serviceName = newServiceName
        , accountName = newAccountName
        , encryptedPassword = newEncryptedPwd
        }

deletePassword :: Session -> T.Text -> IO (Result ())
deletePassword session entryId = do
  let username = Core.Types.username $ sessionUser session
  vaultResult <- loadVault username
  case vaultResult of
    Error err -> return $ Error $ T.append (T.pack "Failed to load vault: ") err
    Success vault -> do
      case find (\e -> entryId == Core.Types.entryId e) (entries vault) of
        Nothing -> return $ Error $ T.pack "Password entry not found"
        Just _ -> do
          let updatedEntries = filter (\e -> entryId /= Core.Types.entryId e) (entries vault)
              updatedVault = vault { entries = updatedEntries }
          
          result <- saveVault updatedVault
          case result of
            Error err -> return $ Error $ T.append (T.pack "Failed to save vault: ") err
            Success _ -> return $ Success ()

listPasswords :: Session -> IO (Result [PasswordEntry])
listPasswords session = do
  let username = Core.Types.username $ sessionUser session
  vaultResult <- loadVault username
  case vaultResult of
    Error err -> return $ Error $ T.append (T.pack "Failed to load vault: ") err
    Success vault -> return $ Success $ entries vault

generatePassword :: Int -> Bool -> Bool -> Bool -> Bool -> IO T.Text
generatePassword length includeUpper includeLower includeDigits includeSpecial = do
  let upperChars = if includeUpper then ['A'..'Z'] else []
      lowerChars = if includeLower then ['a'..'z'] else []
      digitChars = if includeDigits then ['0'..'9'] else []
      specialChars = if includeSpecial then "!@#$%^&*()-_=+[]{}|;:,.<>?/" else []
      allChars = upperChars ++ lowerChars ++ digitChars ++ specialChars
  
  if null allChars
    then return $ T.pack "Error: Must include at least one character set"
    else do
      chars <- replicateM length (randomChar allChars)
      return $ T.pack chars

randomChar :: [Char] -> IO Char
randomChar chars = do
  idx <- randomRIO (0, length chars - 1)
  return $ chars !! idx