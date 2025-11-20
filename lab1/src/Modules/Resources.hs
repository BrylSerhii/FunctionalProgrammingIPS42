{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modules.Resources (resourceLoop) where

import Control.Monad.State
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result (convert)
import Data.Text (Text)
import qualified Data.Text as T
import Types
import Utils

data Resource = Resource
  { rId          :: Int
  , rName        :: Text
  , rAuthorId    :: Int
  , rAnnotation  :: Text
  , rCreateDate  :: Text 
  , rExpireDate  :: Text
  , rTerms       :: Text
  , rUrl         :: Text
  , rTypeId      :: Int
  } deriving (Show)

instance QueryResults Resource where
  convertResults [f1, f2, f3, f4, f5, f6, f7, f8, f9] [v1, v2, v3, v4, v5, v6, v7, v8, v9] = 
      Resource (convert f1 v1) (convert f2 v2) (convert f3 v3) (convert f4 v4) 
               (convert f5 v5) (convert f6 v6) (convert f7 v7) (convert f8 v8) (convert f9 v9)
  convertResults fs vs = error $ "Resource: Column count mismatch."

viewResources :: AppM ()
viewResources = do
    conn <- gets dbConnection
    res <- liftIO $ query_ conn "SELECT resource_id, resource_name, author_id, annotation, creation_date, expiration_date, terms_and_conditions, url_address, resource_type_id FROM Resources" :: AppM [Resource]
    liftIO $ do
      putStrLn "\n--- Resources Table ---"
      putStrLn "ID\tName\tAuthor\tType\tURL"
      mapM_ printRes res
  where
    printRes r = putStrLn $ show (rId r) ++ "\t" ++ T.unpack (rName r) ++ "\t" ++ show (rAuthorId r) ++ "\t" ++ show (rTypeId r) ++ "\t" ++ T.unpack (rUrl r)

createResource :: AppM ()
createResource = do
    name    <- liftIO $ readLineNonEmpty "Resource Name:"
    authId  <- liftIO $ readInt "Author ID:"
    annot   <- liftIO $ readLineNonEmpty "Annotation:"
    cDate   <- liftIO $ readLineNonEmpty "Creation Date (YYYY-MM-DD):"
    eDate   <- liftIO $ readLineNonEmpty "Expiration Date (YYYY-MM-DD):"
    terms   <- liftIO $ readLineNonEmpty "Terms:"
    url     <- liftIO $ readLineNonEmpty "URL:"
    typeId  <- liftIO $ readInt "Type ID:"
    conn <- gets dbConnection
    liftIO $ execute conn "INSERT INTO Resources (resource_name, author_id, annotation, creation_date, expiration_date, terms_and_conditions, url_address, resource_type_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" 
        (name, authId, annot, cDate, eDate, terms, url, typeId)
    liftIO $ putStrLn "Resource added."

deleteResource :: AppM ()
deleteResource = do
    rid <- liftIO $ readInt "Enter Resource ID to delete:"
    conn <- gets dbConnection
    liftIO $ execute conn "DELETE FROM Resources WHERE resource_id = ?" (Only rid)
    liftIO $ putStrLn "Resource deleted."

resourceLoop :: AppM ()
resourceLoop = do
    liftIO $ putStrLn "\n--- Resources Menu ---"
    liftIO $ putStrLn "1. View Resources"
    liftIO $ putStrLn "2. Add Resource"
    liftIO $ putStrLn "3. Delete Resource"
    liftIO $ putStrLn "0. Back"
    choice <- liftIO $ readLineNonEmpty "Choice:"
    case choice of
        "1" -> viewResources >> resourceLoop
        "2" -> createResource >> resourceLoop
        "3" -> deleteResource >> resourceLoop
        "0" -> return ()
        _   -> liftIO (putStrLn "Invalid choice") >> resourceLoop