module Main where

import UI.Console (runConsoleUI)
import Persistence.Storage (ensureDataDirectories)
import System.Directory (doesDirectoryExist)
import Options.Applicative
import qualified Data.Text as T

data Options = Options                                        -- | Command line options
  { optAction :: Action
  }

data Action                                                   -- | Available actions
  = RunConsole                                                -- Run the console UI

optionsParser :: Parser Options                               -- | Command line parser
optionsParser = Options <$> actionParser

actionParser :: Parser Action                                 -- | Action parser
actionParser = pure RunConsole                                -- For now, we only have the console UI

main :: IO ()                                                 -- | Main entry point
main = do
  options <- execParser opts                                  -- Parse command line options
  
  ensureDataDirectories                                       -- Ensure data directories exist
  
  case optAction options of                                   -- Execute the requested action
    RunConsole -> runConsoleUI
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Secure Password Manager"
      <> header "password-manager - A secure encrypted password manager" )