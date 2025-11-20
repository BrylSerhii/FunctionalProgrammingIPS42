{-# LANGUAGE OverloadedStrings #-}

module Modules.Stats (statisticsLoop, resourceStatLinkLoop) where

import Control.Monad.State
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result (convert)
import Types
import Utils

-- === Data Types ===
data Stat = Stat { sId :: Int, sVisitors :: Int, sViews :: Int, sPeak :: Int } deriving (Show)

instance QueryResults Stat where
  convertResults [f1, f2, f3, f4] [v1, v2, v3, v4] = 
      Stat (convert f1 v1) (convert f2 v2) (convert f3 v3) (convert f4 v4)
  convertResults _ _ = error "Stat: Column count mismatch"

data StatLink = StatLink { slResId :: Int, slStatId :: Int } deriving (Show)

instance QueryResults StatLink where
  convertResults [f1, f2] [v1, v2] = StatLink (convert f1 v1) (convert f2 v2)
  convertResults _ _ = error "StatLink: Column count mismatch"

-- === Statistics Operations ===
viewStats :: AppM ()
viewStats = do
    conn <- gets dbConnection
    stats <- liftIO $ query_ conn "SELECT stat_id, unique_visitors, page_views, peak_load FROM UsageStat" :: AppM [Stat]
    liftIO $ do
        putStrLn "\n--- Usage Statistics ---"
        putStrLn "ID\tVisitors\tViews\tPeak"
        mapM_ (\s -> putStrLn $ show (sId s) ++ "\t" ++ show (sVisitors s) ++ "\t" ++ show (sViews s) ++ "\t" ++ show (sPeak s)) stats

createStat :: AppM ()
createStat = do
    uv <- liftIO $ readInt "Unique Visitors:"
    pv <- liftIO $ readInt "Page Views:"
    pl <- liftIO $ readInt "Peak Load:"
    conn <- gets dbConnection
    liftIO $ execute conn "INSERT INTO UsageStat (unique_visitors, page_views, peak_load) VALUES (?, ?, ?)" (uv, pv, pl)
    liftIO $ putStrLn "Statistic added."

deleteStat :: AppM ()
deleteStat = do
    sid <- liftIO $ readInt "Stat ID to delete:"
    conn <- gets dbConnection
    liftIO $ execute conn "DELETE FROM UsageStat WHERE stat_id = ?" (Only sid)
    liftIO $ putStrLn "Statistic deleted."

statisticsLoop :: AppM ()
statisticsLoop = do
    liftIO $ putStrLn "\n--- Statistics Menu ---"
    liftIO $ putStrLn "1. View Stats"
    liftIO $ putStrLn "2. Add Stat"
    liftIO $ putStrLn "3. Delete Stat"
    liftIO $ putStrLn "0. Back"
    choice <- liftIO $ readLineNonEmpty "Choice:"
    case choice of
        "1" -> viewStats >> statisticsLoop
        "2" -> createStat >> statisticsLoop
        "3" -> deleteStat >> statisticsLoop
        "0" -> return ()
        _   -> liftIO (putStrLn "Invalid choice") >> statisticsLoop

-- === Link Operations ===
viewLinks :: AppM ()
viewLinks = do
    conn <- gets dbConnection
    links <- liftIO $ query_ conn "SELECT resource_id, stat_id FROM ResourceStat" :: AppM [StatLink]
    liftIO $ do
        putStrLn "\n--- Resource-Stat Links ---"
        putStrLn "ResourceID\tStatID"
        mapM_ (\l -> putStrLn $ show (slResId l) ++ "\t" ++ show (slStatId l)) links

createLink :: AppM ()
createLink = do
    rid <- liftIO $ readInt "Resource ID:"
    sid <- liftIO $ readInt "Stat ID:"
    conn <- gets dbConnection
    liftIO $ execute conn "INSERT INTO ResourceStat (resource_id, stat_id) VALUES (?, ?)" (rid, sid)
    liftIO $ putStrLn "Link added."

resourceStatLinkLoop :: AppM ()
resourceStatLinkLoop = do
    liftIO $ putStrLn "\n--- Link Menu ---"
    liftIO $ putStrLn "1. View Links"
    liftIO $ putStrLn "2. Create Link"
    liftIO $ putStrLn "0. Back"
    choice <- liftIO $ readLineNonEmpty "Choice:"
    case choice of
        "1" -> viewLinks >> resourceStatLinkLoop
        "2" -> createLink >> resourceStatLinkLoop
        "0" -> return ()
        _   -> liftIO (putStrLn "Invalid choice") >> resourceStatLinkLoop