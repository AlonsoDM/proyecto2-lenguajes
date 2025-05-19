module UI.Console
  ( runConsoleUI
  ) where

import Core.Types
import Features.Auth
import Features.PasswordManager
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)
import Control.Monad (forever, void, when)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import System.Clipboard (setClipboardString)
import System.Exit (exitSuccess)
import Data.List (sortOn)
import Data.Ord (Down(..))


------------------------------
-- User interface functions --
------------------------------


runConsoleUI :: IO ()                                                                               -- Run the console UI
runConsoleUI = do
  putStrLn "===== Secure Password Manager ====="
  putStrLn "Welcome! Please log in or register."
  showLoginMenu

showLoginMenu :: IO ()                                                                              -- Display login menu
showLoginMenu = do
  putStrLn "\n=== Login Menu ==="
  putStrLn "1. Login"
  putStrLn "2. Register"
  putStrLn "3. Exit"
  putStr "Enter your choice: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> handleLogin
    "2" -> handleRegister
    "3" -> exitSuccess
    _ -> do
      putStrLn "Invalid choice, please try again."
      showLoginMenu

handleLogin :: IO ()                                                                                -- Handle user login
handleLogin = do
  putStrLn "\n=== Login ==="
  putStr "Username: "
  hFlush stdout
  username <- T.pack <$> getLine
  putStr "PIN: "
  hFlush stdout
  pin <- T.pack <$> getLine
  
  result <- authenticateUser username pin
  case result of
    Error err -> do
      putStrLn $ "Login failed: " ++ T.unpack err
      showLoginMenu
    Success session -> do
      putStrLn $ "Welcome, " ++ T.unpack username ++ "!"
      showMainMenu session

handleRegister :: IO ()                                                                             -- Handle user registration
handleRegister = do
  putStrLn "\n=== Register ==="
  putStr "Username: "
  hFlush stdout
  username <- T.pack <$> getLine
  putStr "PIN (at least 4 characters): "
  hFlush stdout
  pin <- T.pack <$> getLine
  
  result <- registerUser username pin
  case result of
    Error err -> do
      putStrLn $ "Registration failed: " ++ T.unpack err
      showLoginMenu
    Success _ -> do
      putStrLn "Registration successful! Please log in."
      showLoginMenu

showMainMenu :: Session -> IO ()                                                                    -- Display main menu after login
showMainMenu session = do
  putStrLn "\n=== Main Menu ==="
  putStrLn "1. List passwords"
  putStrLn "2. Add password"
  putStrLn "3. View password details"
  putStrLn "4. Generate password"
  putStrLn "5. Change PIN"
  putStrLn "6. Logout"
  putStr "Enter your choice: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> handleListPasswords session
    "2" -> handleAddPassword session
    "3" -> handleViewPassword session
    "4" -> handleGeneratePassword session
    "5" -> handleChangePIN session
    "6" -> do
      putStrLn "Logged out successfully."
      showLoginMenu
    _ -> do
      putStrLn "Invalid choice, please try again."
      showMainMenu session

handleListPasswords :: Session -> IO ()                                                             -- Handle listing all passwords
handleListPasswords session = do
  putStrLn "\n=== Password List ==="
  result <- listPasswords session
  case result of
    Error err -> do
      putStrLn $ "Failed to list passwords: " ++ T.unpack err
      showMainMenu session
    Success entries -> do
      if null entries
        then putStrLn "No passwords saved yet."
        else do
          putStrLn $ formatEntryHeader
          mapM_ (putStrLn . formatEntryForList) 
                (zip [1..] $ sortOn serviceName entries)
      waitForEnter session

handleAddPassword :: Session -> IO ()                                                               -- Handle adding a new password
handleAddPassword session = do
  putStrLn "\n=== Add New Password ==="
  putStr "Service name: "
  hFlush stdout
  serviceName <- T.pack <$> getLine
  
  putStr "Account name/email: "
  hFlush stdout
  accountName <- T.pack <$> getLine
  
  putStr "Password (leave empty to generate): "
  hFlush stdout
  passwordInput <- getLine
  password <- if null passwordInput
                then do
                  putStrLn "Generating secure password..."
                  generatePassword 16 True True True True
                else return $ T.pack passwordInput
  
  result <- addPassword session serviceName accountName password
  case result of
    Error err -> putStrLn $ "Failed to add password: " ++ T.unpack err
    Success entry -> putStrLn "Password added successfully!"
  
  waitForEnter session

handleViewPassword :: Session -> IO ()                                                              -- Handle viewing password details and actions (copy, edit, delete)
handleViewPassword session = do
  putStrLn "\n=== View Password ==="
  result <- listPasswords session
  case result of
    Error err -> do
      putStrLn $ "Failed to list passwords: " ++ T.unpack err
      showMainMenu session
    Success entries -> do
      if null entries
        then do
          putStrLn "No passwords saved yet."
          waitForEnter session
        else do
          putStrLn "Select a password to view:"
          mapM_ (\(idx, entry) -> 
                  putStrLn $ show idx ++ ". " ++ T.unpack (serviceName entry)) 
                (zip [1..] entries)
          
          putStr "Enter number (0 to cancel): "
          hFlush stdout
          idxStr <- getLine
          case reads idxStr of
            [(idx, "")] | idx > 0 && idx <= length entries -> do
              let entry = entries !! (idx - 1)
              showPasswordDetails session entry
            [(0, "")] -> showMainMenu session
            _ -> do
              putStrLn "Invalid selection."
              handleViewPassword session

showPasswordDetails :: Session -> PasswordEntry -> IO ()                                            -- Show details for a single password entry
showPasswordDetails session entry = do
  putStrLn $ "\n=== " ++ T.unpack (serviceName entry) ++ " ==="
  putStrLn $ "Service: " ++ T.unpack (serviceName entry)
  putStrLn $ "Account: " ++ T.unpack (accountName entry)
  
  pwdResult <- getPassword session (entryId entry)                                                    -- Get and decrypt the password
  case pwdResult of
    Error err -> putStrLn $ "Failed to decrypt password: " ++ T.unpack err
    Success (_, decryptedPwd) -> do
      putStrLn $ "Password: " ++ T.unpack (maskPassword decryptedPwd)
      putStrLn $ "Created: " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (createdAt entry)
      putStrLn $ "Updated: " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (updatedAt entry)
      
      showPasswordActions session entry decryptedPwd                                                -- Show password actions menu

showPasswordActions :: Session -> PasswordEntry -> T.Text -> IO ()                                  -- Show actions for a password entry
showPasswordActions session entry decryptedPwd = do
  putStrLn "\nActions:"
  putStrLn "1. Copy password to clipboard"
  putStrLn "2. Show password"
  putStrLn "3. Edit password"
  putStrLn "4. Delete password"
  putStrLn "5. Back to main menu"
  
  putStr "Enter choice: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      setClipboardString (T.unpack decryptedPwd)
      putStrLn "Password copied to clipboard!"
      waitForEnter session
    
    "2" -> do
      putStrLn $ "Password: " ++ T.unpack decryptedPwd
      waitForEnter session
    
    "3" -> handleEditPassword session entry
    
    "4" -> do
      putStr "Are you sure you want to delete this password? (y/n): "
      hFlush stdout
      confirm <- getLine
      if confirm == "y" || confirm == "Y"
        then do
          result <- deletePassword session (entryId entry)
          case result of
            Error err -> putStrLn $ "Failed to delete password: " ++ T.unpack err
            Success _ -> putStrLn "Password deleted successfully!"
          waitForEnter session
        else showPasswordDetails session entry
    
    "5" -> showMainMenu session
    
    _ -> do
      putStrLn "Invalid choice."
      showPasswordActions session entry decryptedPwd

handleEditPassword :: Session -> PasswordEntry -> IO ()
handleEditPassword session entry = do
  putStrLn "\n=== Edit Password ==="
  putStrLn "Leave fields empty to keep current value."

  putStr $ "New service name [" ++ T.unpack (serviceName entry) ++ "]: "
  hFlush stdout
  newService <- T.pack <$> getLine
  let serviceVal = if T.null newService then Nothing else Just newService

  putStr $ "New account name/email [" ++ T.unpack (accountName entry) ++ "]: "
  hFlush stdout
  newAccount <- T.pack <$> getLine
  let accountVal = if T.null newAccount then Nothing else Just newAccount

  putStr $ "New password (leave empty to keep current): "
  hFlush stdout
  pwdInput <- getLine
  newPwd <- if null pwdInput
    then return Nothing
    else return (Just $ T.pack pwdInput)

  result <- updatePassword session (entryId entry) serviceVal accountVal newPwd
  case result of
    Error err -> putStrLn $ "Failed to update password: " ++ T.unpack err
    Success _ -> putStrLn "Password updated successfully!"

  waitForEnter session

handleGeneratePassword :: Session -> IO ()                                                          -- Handle generating a password without saving
handleGeneratePassword session = do
  putStrLn "\n=== Generate Password ==="
  putStr "Password length (default 16): "
  hFlush stdout
  lengthInput <- getLine
  let pwdLength = if null lengthInput 
                  then 16 
                  else read lengthInput :: Int
  
  putStr "Include uppercase letters (Y/n): "
  hFlush stdout
  upperInput <- getLine
  let includeUpper = upperInput /= "n" && upperInput /= "N"
  
  putStr "Include lowercase letters (Y/n): "
  hFlush stdout
  lowerInput <- getLine
  let includeLower = lowerInput /= "n" && lowerInput /= "N"
  
  putStr "Include numbers (Y/n): "
  hFlush stdout
  numbersInput <- getLine
  let includeNumbers = numbersInput /= "n" && numbersInput /= "N"
  
  putStr "Include special characters (Y/n): "
  hFlush stdout
  specialInput <- getLine
  let includeSpecial = specialInput /= "n" && specialInput /= "N"
  
  password <- generatePassword pwdLength includeUpper includeLower includeNumbers includeSpecial
  putStrLn $ "Generated password: " ++ T.unpack password
  
  putStr "Copy to clipboard? (Y/n): "
  hFlush stdout
  copyInput <- getLine
  when (copyInput /= "n" && copyInput /= "N") $ do
    setClipboardString (T.unpack password)
    putStrLn "Password copied to clipboard!"
  
  waitForEnter session

handleChangePIN :: Session -> IO ()                                                                 -- Handle changing user PIN
handleChangePIN session = do
  putStrLn "\n=== Change PIN ==="
  putStr "Current PIN: "
  hFlush stdout
  currentPin <- T.pack <$> getLine
  
  putStr "New PIN (at least 4 characters): "
  hFlush stdout
  newPin <- T.pack <$> getLine
  
  putStr "Confirm new PIN: "
  hFlush stdout
  confirmPin <- T.pack <$> getLine
  
  if newPin /= confirmPin
    then do
      putStrLn "PINs do not match. Please try again."
      waitForEnter session
    else do
      result <- changePin session currentPin newPin
      case result of
        Error err -> putStrLn $ "Failed to change PIN: " ++ T.unpack err
        Success newSession -> do
          putStrLn "PIN changed successfully!"
          showMainMenu newSession

formatEntryForList :: (Int, PasswordEntry) -> String                                                -- Format a password entry for list display
formatEntryForList (idx, entry) =
  padRight 4 (show idx ++ ".") ++
  padRight 25 (T.unpack $ serviceName entry) ++
  padRight 30 (T.unpack $ accountName entry) ++
  formatTime defaultTimeLocale "%Y-%m-%d" (updatedAt entry)

formatEntryHeader :: String                                                                         -- Format the header row for the password list
formatEntryHeader =
  padRight 4 "#" ++
  padRight 25 "Service" ++
  padRight 30 "Account" ++
  "Last Updated"

maskPassword :: T.Text -> T.Text                                                                    -- Mask a password with asterisks, showing only the first and last character
maskPassword pwd
  | T.length pwd <= 2 = pwd
  | otherwise = T.take 1 pwd `T.append` T.replicate (T.length pwd - 2) (T.pack "*") `T.append` T.takeEnd 1 pwd

padRight :: Int -> String -> String                                                                 -- Right-pad a string to a specified length
padRight n s = s ++ replicate (n - length s) ' '

waitForEnter :: Session -> IO ()                                                                    -- Wait for the user to press Enter, then return to the main menu
waitForEnter session = do
  putStr "\nPress Enter to continue..."
  hFlush stdout
  _ <- getLine
  showMainMenu session