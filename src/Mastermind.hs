module Mastermind
    ( mastermind
    ) where

import System.Random
import Text.Read
import Data.Maybe

data Board a = Board [a]
    deriving (Show)

data Result = Result { boardSize :: Int
                     , correct :: Int
                     , misplaced :: Int
                     } deriving (Eq, Show)

hasWin :: Result -> Bool
hasWin r = (correct r) == (boardSize r)

getResult :: Eq a => Board a -> Board a -> Result
getResult (Board c) (Board b) = go b c c $ Result 0 0 0
    where
        go :: Eq a => [a] -> [a] -> [a] -> Result -> Result
        go [] _ _ r = r
        go _ [] _ r = r
        go (c:cs) (b:bs) xs r
            | c == b      = go cs bs xs $ Result (1 + boardSize r) (1 + correct r) (misplaced r)
            | c `elem` xs = go cs bs xs $ Result (1 + boardSize r) (correct r) (1 + misplaced r)
            | otherwise   = go cs bs xs $ Result (1 + boardSize r) (correct r) (misplaced r)

randomBoard :: Int -> Int -> IO (Board Int)
randomBoard n m = do
    seed <- newStdGen
    pure . Board $ fmap (`mod` n) $ take m $ (randoms seed :: [Int]) 
        

inputBoard :: (Read a, Enum a) => Int -> Int -> IO (Board a)
inputBoard n v = do
    putStrLn $ "Please enter your guess (" ++ (show n) ++ " inputs between 0 and " ++ (show $ v - 1) ++ "): "
    input <- getLine
    let xs = mapM readMaybe $ words input
    if (isNothing xs) || (length $ fromJust xs) /= n || (any (\x -> x < 0 || x >= v) $ map fromEnum $ fromJust xs)
        then do
            putStrLn "Oops, wrong number of inputs (makes sure to separate them with spaces)"
            inputBoard n v
        else
            pure $ Board $ fromJust xs

loop :: Int -> IO Bool -> IO Bool
loop n f = do
    putStrLn $ (show n) ++ " remaining turns" 
    r <- f
    if r || (n <= 1)
        then 
            pure r
        else
            loop (n - 1) f

turn :: (Eq a, Read a, Enum a) => Int -> Board a -> IO Result
turn v b@(Board xs) = do
    let len = length xs
    input <- inputBoard len v
    pure $ getResult b input

prettyPrintResult :: Result -> IO ()
prettyPrintResult r@(Result _ c m) = do
    if (hasWin r)
        then 
            putStrLn "You won, congrats !"
        else do
            putStrLn $ (show c) ++ " are correctly placed"
            putStrLn $ (show m) ++ " are misplaced"

mastermind :: Int -> Int -> Int -> IO Bool
mastermind turns size values = do
    board <- randomBoard values size
    w <- loop turns (doTurn board)
    if w
        then
            pure w
        else do
            putStrLn $ "Solution was " ++ (show board)
            pure w
    where
        doTurn :: Board Int -> IO Bool
        doTurn board = do 
            r <- turn values board
            prettyPrintResult r
            pure $ hasWin r