module Main where

import Control.Concurrent (forkIO, setNumCapabilities, getNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when, forM_)
import Data.Bits (shiftR, (.&.))
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import System.Random (newStdGen, randomRs)
import System.IO (hFlush, stdout)

-- | Helper: Run two IO actions in parallel using a simple fork-join pattern.
-- We use this to distribute the sorting tasks across threads.
parIO :: IO () -> IO () -> IO ()
parIO action1 action2 = do
    done <- newEmptyMVar
    _ <- forkIO $ do
        action1
        putMVar done ()
    action2
    takeMVar done

-- | Compare and swap elements at indices i and j if needed.
-- This is the fundamental "comparator" of the sorting network.
cmpSwap :: MV.IOVector Int -> Int -> Int -> IO ()
cmpSwap arr i j = do
    valI <- MV.read arr i
    valJ <- MV.read arr j
    when (valI > valJ) $ do
        MV.write arr i valJ
        MV.write arr j valI

-- | Batcher's Odd-Even Merge
-- r: The stride (step size)
-- d: Parallel depth counter (to prevent creating too many small threads)
oeMerge :: MV.IOVector Int -> Int -> Int -> Int -> Int -> IO ()
oeMerge arr low n r d = do
    let m = r * 2
    if m < n
        then do
            -- Divide step: effectively merge odd and even subsequences
            -- If we haven't reached depth limit, parallelize the branches
            if d > 0
                then parIO (oeMerge arr low n m (d-1)) 
                           (oeMerge arr (low + r) n m (d-1))
                else do
                    oeMerge arr low n m d
                    oeMerge arr (low + r) n m d
            
            -- Compare-swap step
            -- Iterate starting from (low + r), stepping by m
            let loop i = when (i + r < low + n) $ do
                            cmpSwap arr i (i + r)
                            loop (i + m)
            loop (low + r)
        else
            -- Base case: stride is large, just perform one compare-swap
            cmpSwap arr low (low + r)

-- | Batcher's Odd-Even Merge Sort
-- This is the main recursive driver.
oeSort :: MV.IOVector Int -> Int -> Int -> Int -> IO ()
oeSort arr low n d = 
    if n > 1
        then do
            let m = n `div` 2
            -- Recursively sort the two halves
            if d > 0
                then parIO (oeSort arr low m (d-1)) 
                           (oeSort arr (low + m) m (d-1))
                else do
                    oeSort arr low m d
                    oeSort arr (low + m) m d
            
            -- Merge the sorted halves
            oeMerge arr low n 1 d
        else return ()

-- | Utility: Check if a list is sorted
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- | Utility: Pad input to the next power of 2 (Required for standard Batcher's)
nextPowerOfTwo :: Int -> Int
nextPowerOfTwo x = if x <= 0 then 1 else loop 1
  where loop n = if n >= x then n else loop (n * 2)

main :: IO ()
main = do
    putStr "Enter number of threads (k): "
    hFlush stdout
    inputK <- getLine
    let k = read inputK :: Int

    -- 1. Set the number of capabilities (threads) dynamically
    setNumCapabilities k
    putStrLn $ "--> System configured to use " ++ show k ++ " OS threads."

    putStr "Enter array size (will be padded to power of 2): "
    hFlush stdout
    inputN <- getLine
    let rawN = read inputN :: Int
    let n = nextPowerOfTwo rawN

    putStrLn $ "--> Generating " ++ show n ++ " random integers..."
    gen <- newStdGen
    let randomInts = take n $ randomRs (1, 1000) gen :: [Int]
    
    -- Create a Mutable Vector from the list
    vec <- V.thaw $ V.fromList randomInts

    putStrLn "--> Starting Parallel Batcher's Odd-Even Sort..."
    
    -- We use a depth heuristic: we only spawn parallel tasks for the top levels 
    -- to match the number of requested threads roughly (log2 k).
    let depth = ceiling (logBase 2 (fromIntegral k)) :: Int
    
    oeSort vec 0 n depth

    -- Freeze vector back to immutable to print/verify
    sortedVec <- V.freeze vec
    let sortedList = V.toList sortedVec

    -- Print first 20 elements to verify
    putStrLn "--> Sort Complete."
    putStrLn $ "First 20 elements: " ++ show (take 20 sortedList)
    
    let valid = isSorted sortedList
    putStrLn $ "Verification: Array is sorted? " ++ show valid