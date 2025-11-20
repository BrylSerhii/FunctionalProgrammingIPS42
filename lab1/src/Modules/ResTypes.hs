{-# LANGUAGE OverloadedStrings #-}

module Modules.ResTypes (resourceTypeLoop) where

import Control.Monad.State
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result (convert)
import Data.Text (Text)
import qualified Data.Text as T
import Types
import Utils

data ResType = ResType { rtId :: Int, rtDesc :: Text } deriving (Show)

instance QueryResults ResType where
  convertResults [f1, f2] [v1, v2] = ResType (convert f1 v1) (convert f2 v2)
  convertResults _ _ = error "ResType: Column count mismatch"

viewTypes :: AppM ()
viewTypes = do
    conn <- gets dbConnection
    types <- liftIO $ query_ conn "SELECT type_id, resource_type FROM ResourceTypes" :: AppM [ResType]
    liftIO $ do
        putStrLn "\n--- Resource Types ---"
        putStrLn "ID\tDescription"
        mapM_ (\t -> putStrLn $ show (rtId t) ++ "\t" ++ T.unpack (rtDesc t)) types

createType :: AppM ()
createType = do
    desc <- liftIO $ readLineNonEmpty "Type Name/Description:"
    conn <- gets dbConnection
    liftIO $ execute conn "INSERT INTO ResourceTypes (resource_type) VALUES (?)" (Only desc)
    liftIO $ putStrLn "Type added."

deleteType :: AppM ()
deleteType = do
    tid <- liftIO $ readInt "Type ID to delete:"
    conn <- gets dbConnection
    liftIO $ execute conn "DELETE FROM ResourceTypes WHERE type_id = ?" (Only tid)
    liftIO $ putStrLn "Type deleted."

resourceTypeLoop :: AppM ()
resourceTypeLoop = do
    liftIO $ putStrLn "\n--- Resource Types Menu ---"
    liftIO $ putStrLn "1. View Types"
    liftIO $ putStrLn "2. Add Type"
    liftIO $ putStrLn "3. Delete Type"
    liftIO $ putStrLn "0. Back"
    choice <- liftIO $ readLineNonEmpty "Choice:"
    case choice of
        "1" -> viewTypes >> resourceTypeLoop
        "2" -> createType >> resourceTypeLoop
        "3" -> deleteType >> resourceTypeLoop
        "0" -> return ()
        _   -> liftIO (putStrLn "Invalid choice") >> resourceTypeLoop