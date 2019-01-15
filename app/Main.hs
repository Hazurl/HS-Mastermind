module Main where

import Mastermind

main :: IO ()
main = do
    mastermind 1 1 2
    putStrLn "\nWanna play another one ? y/n"
    l <- getLine
    if length l == 0 || head l /= 'y'
        then
            pure ()
        else
            main
