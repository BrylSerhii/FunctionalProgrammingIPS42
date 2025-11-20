module Utils where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- | Helper to flush stdout so prompts appear immediately
prompt :: String -> IO ()
prompt text = do
    putStr text
    hFlush stdout

-- | Safe read for Int with prompt
readInt :: String -> IO Int
readInt msg = do
    prompt (msg ++ " ")
    input <- getLine
    case readMaybe input of
        Just x  -> return x
        Nothing -> do
            putStrLn "Invalid number. Please try again."
            readInt msg

-- | Read non-empty string
readLineNonEmpty :: String -> IO String
readLineNonEmpty msg = do
    prompt (msg ++ " ")
    input <- getLine
    if null input
        then do
            putStrLn "Input cannot be empty."
            readLineNonEmpty msg
        else return input