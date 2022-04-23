module Main where

import Data.Char
import System.IO


main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        newline
        putStrLn "     -----  Notes Parser  -----"
        newline
        putStr "Enter filename: "
        fn <- getLine
        content <- readFile fn
        newline
        putStrLn "--------------  Start of Content  --------------"
        newline
        putStr content
        newline
        putStrLn "---------------  End of Content  ---------------"
        newline


newline :: IO ()
newline = putChar '\n'

