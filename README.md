# proyecto2-lenguages

Functional Programming: The objective of this assignment is to familiarize students with program development in Haskell by creating a password manager program.

The purpose of this project is to apply functional programming concepts using the Haskell language by building a secure password management system. Students will
design and implement an application that allows the management of PIN-protected passwords, with encrypted storage of information in files.


## Secure Password Manager
A secure password manager implemented in Haskell that uses AES-256 encryption to protect your passwords.

### Features
- User registration and authentication with PIN
- AES-256 encryption for all stored passwords
- Copy passwords to clipboard
- Console-based user interface

### Project Structure

├── app/
│   └── Main.hs                  # Entry point and UI main
├── src/
│   ├── Core/
│   │   ├── Crypto.hs            # Encryption functions (AES)
│   │   └── Types.hs             # Core types (User, Password)
│   ├── Features/
│   │   ├── Auth.hs              # Authentication and user management
│   │   └── PasswordManager.hs   # Password management
│   ├── Persistence/
│   │   └── Storage.hs           # Store/read encrypted data
│   └── UI/
│       └── Console.hs           # Console UI functions
├── data/                        # Directory to store persistent data
│   ├── users.json               # User database (encrypted)
│   └── *_passwords.json         # Password database (encrypted)


### How It Works

1. Users register with a username and PIN
2. A unique master key is generated for each user and encrypted with their PIN
3. All passwords are encrypted with the user's master key
4. The user must authenticate with their PIN to access their passwords
5. Passwords are never stored in plain text at any point

### Security Features
- AES-256 encryption for all sensitive data
- PIN-based authentication
- Unique master key for each user
- Passwords are always encrypted in storage
- PBKDF2 with SHA-256 for key derivation

### Building and Running
1. Clone the repository
2. Build with Stack: `stack build`
3. Run the application: `stack run`

### Usage
1. Register a new account with a username and PIN (at least 4 characters)
2. Login with your credentials
3. Use the menu to add, view, edit, and manage your passwords
4. All data is automatically encrypted and saved

### Dependencies
- cryptonite (for encryption)
- aeson (for JSON handling)
- text, bytestring (for string handling)
- Clipboard (for clipboard integration)
- optparse-applicative (for command line parsing)
- directory, filepath (for file operations)
- time, uuid (for timestamps and unique IDs)

