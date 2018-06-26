module Main where

import System.Environment
import System.Exit

main :: IO ()
main = 
    do arguments <- getArgs
       progName <- getProgName
       if length arguments /= 1
            then do putStrLn ("usage: " ++ progName ++ " filename")
                    exitFailure
            else do text <- readFile (head arguments)
                    mapM_ putStrLn (reverse (lines text))
                    exitSuccess