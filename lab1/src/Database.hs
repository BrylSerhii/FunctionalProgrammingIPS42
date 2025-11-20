{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.MySQL.Simple
import Types

-- | Initialize DB connection
initDB :: IO AppState
initDB = do
    putStrLn "Connecting to MySQL..."
    conn <- connect defaultConnectInfo
      { connectHost     = "127.0.0.1"     -- Matches your Workbench/Docker IP
      , connectUser     = "serhii"        -- Matches your Workbench/Docker User
      , connectPassword = "password123"   -- Matches your Docker Password
      , connectDatabase = "info_res"
      }
    putStrLn "Connected successfully."
    return $ AppState { dbConnection = conn }

-- | Close connection
closeDB :: AppState -> IO ()
closeDB st = do
    close (dbConnection st)
    putStrLn "Connection closed."