module Main where

import Control.Monad.State
import GHC.IO.Encoding (setLocaleEncoding, utf8) -- Import encoding
import Types
import Database
import Utils

-- Importing our modules
import Modules.Users (userLoop)
import Modules.Resources (resourceLoop)
import Modules.ResTypes (resourceTypeLoop)
import Modules.Stats (statisticsLoop, resourceStatLinkLoop)

mainLoop :: AppM ()
mainLoop = do
    liftIO $ putStrLn "\n=== MAIN MENU ==="
    liftIO $ putStrLn "1. Manage Resources"
    liftIO $ putStrLn "2. Manage Users"
    liftIO $ putStrLn "3. Manage Resource/Stat Links"
    liftIO $ putStrLn "4. Manage Statistics"
    liftIO $ putStrLn "5. Manage Resource Types"
    liftIO $ putStrLn "0. Exit"
    
    choice <- liftIO $ readLineNonEmpty "Choice:"
    case choice of
        "1" -> resourceLoop >> mainLoop
        "2" -> userLoop >> mainLoop
        "3" -> resourceStatLinkLoop >> mainLoop
        "4" -> statisticsLoop >> mainLoop
        "5" -> resourceTypeLoop >> mainLoop
        "0" -> liftIO $ putStrLn "Goodbye!"
        _   -> liftIO (putStrLn "Invalid choice") >> mainLoop

main :: IO ()
main = do
    -- FIX: Set encoding to UTF-8 to prevent crashes with Cyrillic text
    setLocaleEncoding utf8

    -- 1. Initialize DB
    initialState <- initDB
    
    -- 2. Run App
    runStateT mainLoop initialState
    
    -- 3. Clean up
    closeDB initialState