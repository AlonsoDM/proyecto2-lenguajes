{-# LANGUAGE OverloadedStrings #-}

module Features.Auth
( registerUser
, authenticateUser
, changePin
, Session(..)
) where

import Core.Types
import Core.Crypto
import Persistence.Storage

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (when, unless)
import Data.List (find)


----------------------------------------
-- Authentication and user management --
----------------------------------------


registerUser :: T.Text -> T.Text -> IO (Result User)
registerUser username pin = do
  -- First validate inputs
  if T.null username
    then return $ Error "Username cannot be empty"
    else if T.length pin < 4
      then return $ Error "PIN must be at least 4 characters long"
      else do
        usersResult <- loadUsers
        case usersResult of
          Error err -> return $ Error $ "Failed to load users: " `T.append` err
          Success users ->
            case find (\u -> username == Core.Types.username u) users of
              Just _ -> return $ Error "Username already exists"
              Nothing -> do
                salt <- generateSalt
                masterKey <- generateMasterKey
                let pinHash = hashPin pin salt
                    derivedKey = deriveKeyFromPin pin salt
                encryptedMasterKey <- encryptMasterKey derivedKey masterKey
                
                let newUser = User
                      { username = username
                      , pinHash = pinHash
                      , salt = salt
                      , masterKey = encryptedMasterKey
                      }
                
                result <- saveUsers (newUser : users)
                case result of
                  Error err -> return $ Error $ "Failed to save user: " `T.append` err
                  Success _ -> do
                    saveResult <- saveVault (Vault username [])
                    case saveResult of
                      Error err -> return $ Error $ "User created but failed to initialize vault: " `T.append` err
                      Success _ -> return $ Success newUser

authenticateUser :: T.Text -> T.Text -> IO (Result Session)
authenticateUser username pin = do
  usersResult <- loadUsers
  case usersResult of
    Error err -> return $ Error $ "Failed to load users: " `T.append` err
    Success users ->
      case find (\u -> username == Core.Types.username u) users of
        Nothing -> return $ Error "User not found"
        Just user -> do
          if verifyPin pin (salt user) (pinHash user)
            then do
              let derivedKey = deriveKeyFromPin pin (salt user)
              case decryptMasterKey derivedKey (masterKey user) of
                Left _ -> return $ Error "Failed to decrypt master key"
                Right decryptedKey ->
                  return $ Success $ Session
                    { sessionUser = user
                    , sessionKey = decryptedKey
                    }
            else return $ Error "Invalid PIN"

changePin :: Session -> T.Text -> T.Text -> IO (Result Session)
changePin session oldPin newPin = do
  let user = sessionUser session
      masterKey = sessionKey session
  
  if not (verifyPin oldPin (salt user) (pinHash user))
    then return $ Error "Current PIN is incorrect"
    else if T.length newPin < 4
      then return $ Error "New PIN must be at least 4 characters long"
      else do
        let newPinHash = hashPin newPin (salt user)
            newDerivedKey = deriveKeyFromPin newPin (salt user)
        
        newEncryptedMasterKey <- encryptMasterKey newDerivedKey masterKey
        
        let updatedUser = user
              { pinHash = newPinHash
              , masterKey = newEncryptedMasterKey
              }
        
        usersResult <- loadUsers
        case usersResult of
          Error err -> return $ Error $ "Failed to load users: " `T.append` err
          Success users -> do
            let updatedUsers = map (\u -> if Core.Types.username u == Core.Types.username user then updatedUser else u) users
            
            result <- saveUsers updatedUsers
            case result of
              Error err -> return $ Error $ "Failed to save updated user: " `T.append` err
              Success _ -> return $ Success $ Session updatedUser masterKey