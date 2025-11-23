module Main where

import System.IO
import Data.List (sortBy)
import Data.Char (isSpace, isPunctuation, toLower)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- ==========================================
-- 1. Type Definitions
-- ==========================================

type Symbol = Char
-- Renamed 'Word' to 'TextWord' to avoid conflict with standard Haskell 'Word' type
type TextWord = String 
type Sentence = [TextWord]

-- ==========================================
-- 2. Text Normalization
-- ==========================================

-- | Replaces tabs and sequences of spaces with a single space.
normalizeText :: String -> String
normalizeText [] = []
normalizeText (x:xs)
    | isSpace x = ' ' : normalizeText (dropWhile isSpace xs)
    | otherwise = x : normalizeText xs

-- | Removes punctuation from a string to extract clean words.
removePunctuation :: String -> String
removePunctuation = filter (not . isPunctuation)

-- ==========================================
-- 3. Parsing Logic
-- ==========================================

-- | Split text into Sentences based on common delimiters (. ! ?)
toSentences :: String -> [String]
toSentences [] = []
toSentences xs = 
    let (sentence, rest) = break (`elem` ".!?") xs
    in if null rest 
       then [sentence] 
       else sentence : toSentences (drop 1 rest)

-- | Convert a raw sentence string into our 'Sentence' type
parseSentence :: String -> Sentence
parseSentence s = 
    let cleanS = removePunctuation s
    in words cleanS -- 'words' splits by space

-- | Main pipeline: Text -> Normalized Text -> [Sentence] -> [TextWord]
extractAllWords :: String -> [TextWord]
extractAllWords text = 
    let normalized = normalizeText text
        rawSentences = toSentences normalized
        sentences = map parseSentence rawSentences :: [Sentence]
    in concat sentences

-- ==========================================
-- 4. Sorting Logic
-- ==========================================

-- | Counts how many times a specific symbol appears in a word (Case Insensitive)
countOccurrences :: Symbol -> TextWord -> Int
countOccurrences target word = 
    length $ filter (\c -> toLower c == toLower target) word

-- | Comparator function: 
-- 1. Compare by count of symbol (Descending: Highest count first)
-- 2. Compare by alphabet (Ascending) if counts are equal
wordComparator :: Symbol -> TextWord -> TextWord -> Ordering
wordComparator target w1 w2
    | count1 > count2 = LT -- We want w1 first (Descending)
    | count1 < count2 = GT -- We want w2 first
    | otherwise       = compare (map toLower w1) (map toLower w2) -- Alphabetical tie-break
  where
    count1 = countOccurrences target w1
    count2 = countOccurrences target w2

-- ==========================================
-- 5. Main Application
-- ==========================================

main :: IO ()
main = do
    -- FIX: Explicitly set encoding for standard input/output handles
    -- This forces the console to accept Cyrillic input regardless of system locale
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    
    -- Also set global default for file reading
    setLocaleEncoding utf8
    
    putStrLn "Reading 'textbook.txt'..."
    -- Read file content
    content <- readFile "textbook.txt"
    
    putStrLn "Enter a symbol to sort by (e.g., 'а', 'о', 'п'): "
    hFlush stdout
    input <- getLine
    
    if null input 
        then putStrLn "Error: You must enter a symbol."
        else do
            let targetSymbol = head input
            
            -- Process the text
            let allWords = extractAllWords content
            
            -- Sort the words
            let sortedWords = sortBy (wordComparator targetSymbol) allWords
            
            putStrLn $ "\n--- Results (Sorted by occurrences of '" ++ [targetSymbol] ++ "') ---"
            printList sortedWords

-- Helper to print list nicely
printList :: [String] -> IO ()
printList [] = return ()
printList xs = mapM_ putStrLn xs