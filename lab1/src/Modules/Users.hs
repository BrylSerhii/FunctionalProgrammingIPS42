{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modules.Users (userLoop) where

import Control.Monad.State
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result (convert)
import Data.Text (Text)
import qualified Data.Text as T
import Types
import Utils

data User = User 
  { userId    :: Int
  , userName  :: Text
  , userEmail :: Text 
  } deriving (Show)

instance QueryResults User where
  -- We must match BOTH the fields list (f) and values list (v)
  convertResults [f1, f2, f3] [v1, v2, v3] = User (convert f1 v1) (convert f2 v2) (convert f3 v3)
  convertResults fs vs = error $ "User: Column count mismatch."

viewAll :: AppM ()
viewAll = do
    conn <- gets dbConnection
    users <- liftIO $ query_ conn "SELECT user_id, user_name, user_email FROM Users" :: AppM [User]
    
    liftIO $ do
      putStrLn "\n--- Users Table ---"
      putStrLn "ID\tName\t\tEmail"
      putStrLn "--------------------------------"
      mapM_ printUser users
  where
    printUser u = putStrLn $ show (userId u) ++ "\t" ++ T.unpack (userName u) ++ "\t" ++ T.unpack (userEmail u)

createItem :: AppM ()
createItem = do
    name  <- liftIO $ readLineNonEmpty "Enter user name:"
    email <- liftIO $ readLineNonEmpty "Enter user email:"
    conn  <- gets dbConnection
    liftIO $ execute conn "INSERT INTO Users (user_name, user_email) VALUES (?, ?)" (name, email)
    liftIO $ putStrLn "User added successfully."

deleteItem :: AppM ()
deleteItem = do
    uid  <- liftIO $ readInt "Enter user ID to delete:"
    conn <- gets dbConnection
    rows <- liftIO $ execute conn "DELETE FROM Users WHERE user_id = ?" (Only uid)
    liftIO $ putStrLn $ "Deleted " ++ show rows ++ " user(s)."

userLoop :: AppM ()
userLoop = do
    liftIO $ putStrLn "\n--- Users Menu ---"
    liftIO $ putStrLn "1. View all users"
    liftIO $ putStrLn "2. Add user"
    liftIO $ putStrLn "3. Delete user"
    liftIO $ putStrLn "0. Back"
    choice <- liftIO $ readLineNonEmpty "Choice:"
    case choice of
        "1" -> viewAll >> userLoop
        "2" -> createItem >> userLoop
        "3" -> deleteItem >> userLoop
        "0" -> return ()
        _   -> liftIO (putStrLn "Invalid choice") >> userLoop