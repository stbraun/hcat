module Main where

import Data.Char
import System.IO
import System.Environment


main :: IO ()
main = do
        args <- getArgs
        let fn = if (length args) < 1
            then error "Path to a note expected!"
            else args !! 0
        hSetBuffering stdout NoBuffering
        newline
        putStrLn "     -----  Notes Parser  -----"
        newline
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

