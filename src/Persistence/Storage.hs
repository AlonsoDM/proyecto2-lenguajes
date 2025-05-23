{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.Storage
  ( ensureDataDirectories
  , saveUsers
  , loadUsers
  , saveVault
  , loadVault
  ) where

import Core.Types
import Core.Crypto

import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON, FromJSON, encode, decode, Value)
import qualified Data.Aeson as Aeson
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Maybe (catMaybes)


------------------------------
-- Save/read encrypted data --
------------------------------


-- Data directory paths
                                      
dataDir :: FilePath                           
dataDir = "data"                              
                                              
usersFile :: FilePath                         
usersFile = dataDir </> "users.json"          
                                              
passwordsFile :: FilePath                     
passwordsFile = dataDir </> "passwords.json"  
                                              


-- Ensure data directories exist
ensureDataDirectories :: IO ()                                                                                  
ensureDataDirectories = do
  createDirectoryIfMissing True dataDir



-- User serialization for storage
data UserDTO = UserDTO                                                                                          
  { dto_username :: T.Text
  , dto_pinHash :: T.Text
  , dto_salt :: T.Text
  , dto_masterKey :: T.Text
  } deriving (Show, Eq, Generic)

instance ToJSON UserDTO
instance FromJSON UserDTO

userToDTO :: User -> UserDTO
userToDTO User{..} = UserDTO
  { dto_username = username
  , dto_pinHash = TE.decodeUtf8 $ B64.encode pinHash
  , dto_salt = TE.decodeUtf8 $ B64.encode salt
  , dto_masterKey = TE.decodeUtf8 $ B64.encode masterKey
  }

dtoToUser :: UserDTO -> Either String User                                                                          -- Convert UserDTO to User after deserialization
dtoToUser UserDTO{..} = do
  pinHashBS <- B64.decode $ TE.encodeUtf8 dto_pinHash
  saltBS <- B64.decode $ TE.encodeUtf8 dto_salt
  masterKeyBS <- B64.decode $ TE.encodeUtf8 dto_masterKey
  return $ User
    { username = dto_username
    , pinHash = pinHashBS
    , salt = saltBS  
    , masterKey = masterKeyBS
    }

saveUsers :: [User] -> IO (Result ())                                                                                -- Save users to file
saveUsers users = do
  ensureDataDirectories
  let userDTOs = map userToDTO users
  result <- try $ LBS.writeFile usersFile (encode userDTOs) :: IO (Either SomeException ())
  case result of
    Left e -> return $ Error $ T.pack $ "Failed to save users: " ++ show e
    Right _ -> return $ Success ()

loadUsers :: IO (Result [User])                                                                                      -- Load users from file
loadUsers = do
  ensureDataDirectories
  exists <- doesFileExist usersFile
  if not exists
    then return $ Success []                                                                                         -- No users file yet
    else do
      result <- try $ LBS.readFile usersFile :: IO (Either SomeException LBS.ByteString)
      case result of
        Left e -> return $ Error $ T.pack $ "Failed to read users file: " ++ show e
        Right contents ->
          case decode contents of
            Nothing -> return $ Error (T.pack "Failed to parse users file")
            Just userDTOs -> do
              let userResults = map dtoToUser userDTOs
                  errors = [err | Left err <- userResults]
                  users = [user | Right user <- userResults]
              if not (null errors)
                then return $ Error $ T.pack $ "Failed to decode some users: " ++ show errors
                else return $ Success users

saveVault :: Vault -> IO (Result ())                                                                                  -- Save vault to file
saveVault vault = do
  ensureDataDirectories
  result <- try $ LBS.writeFile 
                  (vaultFilePath $ owner vault) 
                  (encode vault) :: IO (Either SomeException ())
  case result of
    Left e -> return $ Error $ T.pack $ "Failed to save vault: " ++ show e
    Right _ -> return $ Success ()

loadVault :: T.Text -> IO (Result Vault)                                                                              -- Load vault for a user
loadVault username = do
  let filePath = vaultFilePath username
  exists <- doesFileExist filePath
  if not exists
    then return $ Success $ Vault username []                                                                         -- New empty vault
    else do
      result <- try $ LBS.readFile filePath :: IO (Either SomeException LBS.ByteString)
      case result of
        Left e -> return $ Error $ T.pack $ "Failed to read vault file: " ++ show e
        Right contents ->
          case decode contents of
            Nothing -> return $ Error (T.pack "Failed to parse vault file")
            Just vault -> return $ Success vault

vaultFilePath :: T.Text -> FilePath                                                                                   -- Get the vault file path for a user
vaultFilePath username = dataDir </> (T.unpack username ++ "_passwords.json")