module Core.Crypto
  ( hashPin
  , verifyPin
  , generateSalt
  , generateMasterKey
  , encryptWithKey
  , decryptWithKey
  , deriveKeyFromPin
  , encryptMasterKey
  , decryptMasterKey
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Crypto.Hash as Hash
import Crypto.Random (getRandomBytes)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV, cipherInit)
import Crypto.Error (throwCryptoError, CryptoError(..))
import qualified Crypto.KDF.PBKDF2 as PBKDF2
-- import Crypto.Data.Padding (pad, unpad, PKCS7)
import Crypto.Data.Padding (pad, unpad, Format(PKCS7))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64 as B64



-------------------------------
-- Encryption Features (AES) --
-------------------------------


generateSalt :: IO BS.ByteString                                                                              -- Generate a random salt
generateSalt = getRandomBytes 16

generateMasterKey :: IO BS.ByteString                                                                         -- Generate a random master key for encrypting passwords
generateMasterKey = getRandomBytes 32                                                                         -- 256 bits for AES256

hashPin :: T.Text -> BS.ByteString -> BS.ByteString                                                           -- Hash a PIN with a salt using PBKDF2-HMAC-SHA256, 10,000 iterations, 32-byte output
-- hashPin pin salt = PBKDF2.generate
  -- (PBKDF2.prfHMAC Hash.SHA256)                                                                                
  -- { PBKDF2.iterCounts = 10000                                                                                 
  -- , PBKDF2.outputLength = 32                                                                                  
  -- }
  -- (TE.encodeUtf8 pin)                                                                                         
  -- salt                                                                                                       
hashPin pin salt = PBKDF2.generate
(PBKDF2.prfHMAC Hash.SHA256)                                                                                  -- Use HMAC with SHA256
  (PBKDF2.Parameters
     { PBKDF2.iterCounts = 10000                                                                              -- Number of iterations
  , PBKDF2.outputLength = 32                                                                                  -- Output length in bytes (256 bits)
     })
  (TE.encodeUtf8 pin)                                                                                         -- PIN as input
  salt
  
verifyPin :: T.Text -> BS.ByteString -> BS.ByteString -> Bool                                                 -- Verify if a PIN matches the stored hash
verifyPin pin salt storedHash = hashPin pin salt == storedHash

deriveKeyFromPin :: T.Text -> BS.ByteString -> BS.ByteString                                                  -- Derive an encryption key from the PIN and salt
deriveKeyFromPin = hashPin                                                                                    -- We're using the same PBKDF2 process

encryptMasterKey :: BS.ByteString -> BS.ByteString -> IO BS.ByteString                                        -- Encrypt the master key with a key derived from the PIN
encryptMasterKey derivedKey masterKey = do
  iv <- generateIV
  let encryptedKey = encryptWithKey' derivedKey iv masterKey
  return $ iv `BS.append` encryptedKey                                                                        -- Store IV and encrypted data together

decryptMasterKey :: BS.ByteString -> BS.ByteString -> Either CryptoError BS.ByteString                        -- Decrypt the master key with a key derived from the PIN
-- decryptMasterKey derivedKey encryptedData = do
--   let (iv, encryptedKey) = BS.splitAt 16 encryptedData                                                        -- Split IV and encrypted data
--   decryptWithKey' derivedKey iv encryptedKey
decryptMasterKey derivedKey encryptedData =
  if BS.length encryptedData < 16
    then Left CryptoError_IvSizeInvalid
    else
      let (iv, encryptedKey) = BS.splitAt 16 encryptedData
      in decryptWithKey' derivedKey iv encryptedKey

encryptWithKey :: BS.ByteString -> T.Text -> IO T.Text                                                        -- Encrypt data with the master key
encryptWithKey key plaintext = do
  iv <- generateIV
  let encryptedBS = encryptWithKey' key iv (TE.encodeUtf8 plaintext)
  return $ TE.decodeUtf8 $ B64.encode $ iv `BS.append` encryptedBS                                            -- Encode IV and encrypted data in Base64 for storage


decryptWithKey :: BS.ByteString -> T.Text -> Either CryptoError T.Text                                        -- Decrypt data with the master key
decryptWithKey key ciphertext = do
  case B64.decode (TE.encodeUtf8 ciphertext) of                                                               -- Decode from Base64
    -- Left err -> Left $ CryptoError_SecretKeySizeInvalid
    Left _ -> Left $ CryptoError_MacKeyInvalid
    Right encryptedData -> do
      let (iv, encryptedBS) = BS.splitAt 16 encryptedData                                                     -- Split IV and encrypted data
      decryptedBS <- decryptWithKey' key iv encryptedBS       
      return $ TE.decodeUtf8 decryptedBS                                                                      -- Convert back to Text

generateIV :: IO BS.ByteString                                                                                -- Generate random IV for AES
generateIV = getRandomBytes 16                                                                                -- 16 bytes (128 bits) for AES

encryptWithKey' :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString                           -- Low-level encryption function
-- encryptWithKey' key ivBS plaintext =
--   case makeIV ivBS of
--     Nothing -> error "Failed to create IV"                                                                    -- Should never happen with proper IV size
--     Just iv -> do
--       let cipher = throwCryptoError $ cipherInit key :: AES256
--           paddedPlaintext = pad (PKCS7 16) plaintext
--       --ciphertext $ cbcEncrypt cipher iv paddedPlaintext
--       cbcEncrypt cipher iv paddedPlaintext
encryptWithKey' key ivBS plaintext =
  case makeIV ivBS of
    Nothing -> error "Failed to create IV"
    Just iv ->
      let cipher = throwCryptoError $ cipherInit key :: AES256
          paddedPlaintext = pad (PKCS7 16) plaintext
      in cbcEncrypt cipher iv paddedPlaintext

decryptWithKey' :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either CryptoError BS.ByteString        -- Low-level decryption function
decryptWithKey' key ivBS ciphertext =
  case makeIV ivBS of
    Nothing -> Left $ CryptoError_IvSizeInvalid
    Just iv -> do
      let cipher = throwCryptoError $ cipherInit key :: AES256
          decrypted = cbcDecrypt cipher iv ciphertext
      case unpad (PKCS7 16) decrypted of
        Nothing -> Left $ CryptoError_MacKeyInvalid                                                           -- Padding error
        Just unpaddedText -> Right unpaddedText