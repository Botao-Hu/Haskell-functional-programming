module Lab5 where

import Control.Monad

-- Part A
-- Problem 1
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = 
    do i <- [1..]
       j <- [1..i - 1]
       k <- [1..j - 1]
       l <- [1..k - 1]
       guard $ i^3 + l^3 == j^3 + k^3
       return ((i, l), (j, k), i^3 + l^3)

-- Problem 2
-- Using guard
naturalSum1 :: Integer
naturalSum1 = sum
    (do i <- [0..999]
        guard $ i `mod` 3 == 0 || i `mod` 5 == 0
        return i)

-- Using mzero
naturalSum2 :: Integer
naturalSum2 = sum
    (do i <- [0..999]
        if i `mod` 3 == 0 || i `mod` 5 == 0 
            then return () 
            else mzero
        return i)

-- Problem 3
isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)

largestPalindrome :: Integer
largestPalindrome = maximum
    (do i <- [100..999]
        j <- [100..999]
        guard $ isPalindrome (i * j)
        return (i * j))

-- The largest palindrome made from the product two 3-digit numbers
-- is 906609.

-- Problem 4
type Expr = [Item]

data Item = N Int | O Op
    deriving Show

data Op = Add | Sub | Cat
    deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

-- List all the possible valid expressions from the puzzle.
exprs :: [Expr]
exprs = 
    do op1 <- ops
       op2 <- ops
       op3 <- ops
       op4 <- ops
       op5 <- ops
       op6 <- ops
       op7 <- ops
       op8 <- ops
       return [N 1, op1, N 2, op2, N 3, op3, N 4, op4,
               N 5, op5, N 6, op6, N 7, op7, N 8, op8, N 9]

-- Removes all the Cat operator in the expression, and concatenates digits.
normalize :: Expr -> Expr
normalize ((N x) : (O Cat) : (N y) : xs) = normalize ((N (10 * x + y)) : xs)
normalize ((N x) : (O Add) : xs) = [N x, O Add] ++ (normalize xs)
normalize ((N x) : (O Sub) : xs) = [N x, O Sub] ++ (normalize xs)
normalize ((N x) : []) = [N x]
normalize _ = error "illegitimate patterns"

-- Evaluates a normalized expression and returns its answer.
evaluate :: Expr -> Int
evaluate ((N x) : (O Add) : (N y) : xs) = evaluate ((N (x + y)) : xs)
evaluate ((N x) : (O Sub) : (N y) : xs) = evaluate ((N (x - y)) : xs)
evaluate ((N x) : []) = x
evaluate _ = error "illegitimate patterns"

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs

-- Part B
{-
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f lst = concat (map f lst)

(>>=) :: [a] -> (a -> [b]) -> [b]
mv >>= f = concatMap f mv

Problem 1
If we consider [] as a result of function f
f :: Int -> Int -> [Int]
f x y = []
This expression can be desugared into
[1..6] >>= 
    \n1 -> ([1..6] >>= 
                \n2 -> ([] >> return (n1, n2)))
--> [1..6] >>= 
        \n1 -> ([1..6] >>= 
                    \n2 -> ([] >>= (\_ -> return (n1, n2)))
--> [1..6] >>= 
        \n1 -> ([1..6] >>= 
                    \n2 -> concatMap (\_ -> return (n1, n2)) [])
--> [1..6] >>= \n1 -> ([1..6] >>= \n2 -> [])
--> [1..6] >>= \n1 -> concatMap (\n2 -> []) [1..6]
--> [1..6] >>= \n1 -> []
--> concatMap (\n1 -> []) [1..6]
--> []

Problem 2
The former expression can be desugared into
[1..6] >>= 
    \n1 -> ([1..6] >>= 
                \n2 -> (return <anything> >>= 
                            (\_ -> return (n1, n2))))
--> [1..6] >>= 
        \n1 -> ([1..6] >>= 
                    \n2 -> concatMap (\_ -> return (n1, n2) (return <anything>)))
--> [1..6] >>= 
        \n1 -> ([1..6] >>= 
                    \n2 -> concatMap (\_ -> return (n1, n2) [<anything>]))
--> [1..6] >>= \n1 -> ([1..6] >>= \n2 -> return (n1, n2))
The latter expression can be desugared into
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> return (n1, n2))
which is identical to the former one, so two expressions return the same thing.

Problem 3
The expression can be desugared into (consider fail _ = [])
["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>=
    \y -> case y of
        ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
        _ -> []
--> concatMap 
        (\y -> case y of
            ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
            _ -> [])
        ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
--> concat (map (\y -> case y of
                    ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
                    _ -> [])
            ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])
--> concat [[['x', 'y']], [['z', 'w']], [], [['c', 'c']], []]
--> concat [["xy"], ["zw"], [], ["cc"], []]
--> ["xy", "zw", "cc"]
If we use fail s = error s, then we will abort the computation during the
map process when we come to "foobar". And in the console we can see the
error message "Exception: foobar".

Problem 4
For m = [x1, x2, ...] case:
foldr ((++) . k) [] m
= foldr (\x -> (++) (k x)) [] m
= foldr (\y -> (\x -> (k x) ++ y)) [] m
= foldr (\x y -> (k x) ++ y) [] [x1, x2, ...]
= (k x1) ++ (foldr (\x y -> (k x) ++ y) [] [x2, ...])
= (k x1) ++ (k x2) ++ ... ++ []
= concat [k x1, k x2, ...]
= concat (map k [x1, x2, ...])
= concat (map k m)
So both expression evaluate to the same thing.

Problem 5
The problem is that n and s in AnyNum (n + s) might be different types, say
n is Fractional and s is Integer. In this situation, GHC cannot perform an
operation since addition (+) is not defined for a Fractional type and an
Integer type. (+) is only defined for two arguments that are of the same type
under Num, and that is why GHC raised an error here.
To solve this, Ben should define a new addition operator among all the types
of Num types, and cast result properly if he wishes to get an answer of a 
certain type.
-}