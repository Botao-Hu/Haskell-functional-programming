--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
-- import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
  where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter s (i, j) =
      do tmp <- readArray s (i, j)
         if tmp == 0
            then do okValues <- getOKValues s (i, j)
                    iter' s (i, j) okValues
            else do (i', j') <- findNext s (i, j)
                    if (i', j') == (-1, -1)
                       then return True
                       else iter s (i', j')

    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' s (i, j) (x:xs) = 
      do writeArray s (i, j) x
         (i', j') <- findNext s (i, j)
         -- Go to the next zero, and if there is no zero left,
         -- then we succeed in this Sudoku, return True.
         if (i', j') == (-1, -1)
            then return True
            -- Otherwise, go to the next zero, and calculate
            -- the okValues, and call the next level iter'
            else do okValues <- getOKValues s (i', j')
                    res <- iter' s (i', j') okValues
                    if res == False
                       -- if the value x we wrote leads to failure,
                       -- we erase it and try another value in okValues.
                       then do writeArray s (i, j) 0
                               iter' s (i, j) xs
                       else return True
    -- If there is no legal values, return False
    iter' _ _ [] = return False

    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues s (i, j) = 
      do rowList <- getRow s i
         colList <- getCol s j
         boxList <- getBox s (i, j)
         return (filter (\x -> notElem x (rowList ++ colList ++ boxList)) [1..9])

    -- Return the ith row in a Sudoku board as a list of Ints.
    -- Here, we assume the function caller would use valid row index.
    getRow :: Sudoku -> Int -> IO [Int]
    getRow s row = mapM (\y -> readArray s (row, y)) [1..9]

    -- Return the ith column in a Sudoku board as a list of Ints.
    -- Here, we assume the function caller would use valid column index.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol s col = mapM (\y -> readArray s (y, col)) [1..9]

    -- Return the box covering location (i, j) as a list of Ints.
    -- Here, we assume the function caller would use valid index.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox s (row, col) = 
      do let i = (div (row - 1) 3)
         let j = (div (col - 1) 3)
         mapM (readArray s) [(x, y) | x <- [(3 * i + 1)..(3 * i + 3)], 
                                      y <- [(3 * j + 1)..(3 * j + 3)]]

    -- Find the next index of a given array index that has zero value.
    -- Assume that the function caller would only pass in index
    -- between (1, 1) and (9, 8).
    -- If there is no zero value beyond this index, return (-1, -1).
    findNext :: Sudoku -> (Int, Int) -> IO (Int, Int)
    findNext s (i, j) =
      do if (i, j) >= (9, 9) 
            then return (-1, -1)
            else if j < 9
                    then do tmp <- readArray s (i, j + 1)
                            if tmp /= 0
                               then findNext s (i, j + 1)
                               else return (i, j + 1)
                    else do tmp <- readArray s (i + 1, 1)
                            if tmp /= 0
                               then findNext s (i + 1, 1)
                               else return (i + 1, 1)

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure
