module Main where

import System.Environment
import System.Exit
import Data.Char
import System.IO

main :: IO ()
main = 
    do arguments <- getArgs
       progName <- getProgName
       if isLegalInput (init arguments)
            -- if legal input, get the input txt information
            then do text <- getText (last arguments)
                    -- get the column index list
                    let colList = map (\x -> read x :: Int)
                                      (init arguments)
                    -- use getColumn function to extract corresponding
                    -- word (or do nothing if there is no such column)
                    -- in each line of input txt
                    let newText = map unwords 
                                      (map (getColumn colList) 
                                           (map words (lines text)))
                    mapM_ putStrLn newText
                    exitSuccess
            -- if not legal input, print helper message
            else do helperMsg progName
                    exitFailure

{-
This function determines whether the input arguments (except
the filename) are legal or not, and returns a bool variable.
-}
isLegalInput :: [String] -> Bool
isLegalInput (x:xs) = all isDigit x 
                      && (read x :: Int) > 0
                      && isLegalInput xs
isLegalInput [] = True

{-
This function prints the helper message for invalid argument
input.
-}
helperMsg :: String -> IO ()
helperMsg progName = 
    do putStrLn ("usage: " ++ progName ++ 
                 " n1 n2 ... filename")
       putStrLn ("n1 n2 ... : column numbers of the"
                 ++ " file, need any n > 0")
       putStrLn ("filename: text file, or '-'"
                 ++ " meaning read from stdin")

-- This function gets the filename and returns the text.
getText :: String -> IO String
getText "-" = hGetContents stdin
getText filename = readFile filename

-- This function returns words with given column index.
getColumn :: [Int] -> [String] -> [String]
getColumn (x:xs) y | x <= length y = [y !! (x - 1)] ++ 
                                     getColumn xs y
                   | otherwise = []
getColumn _ _ = []