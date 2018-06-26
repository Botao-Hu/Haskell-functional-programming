module Lab1 where
-- Part B
-- Problem 1.1
(+*) :: Double -> Double -> Double
(+*) x y = x * x + y * y
infixl 7 +*
-- Problem 1.2
(^||) :: Bool -> Bool -> Bool
True ^|| x = not x
False ^|| x = x
infixr 3 ^||
-- Problem 2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | x > y = error "bad input"
rangeProduct x y | x == y = y
rangeProduct x y | otherwise = x * (rangeProduct (x + 1) y)
-- Problem 3
prod :: [Integer] -> Integer
prod = foldr (*) 1
rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 x y | x > y = error "bad input"
rangeProduct2 x y = prod [x..y]
-- Problem 4.1
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
-- Problem 4.2
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ _ _ [] = []
map3 _ _ [] _ = []
map3 _ [] _ _ = []
map3 f (x:xs) (y:ys) (z:zs) = f x y z : map3 f xs ys zs
-- Problem 4.3
{-
dot lst1 lst2
--> ((sum .) . map2 (*)) lst1 lst2
--> ((sum .) (map2 (*) lst1)) lst2 
[f . g x = f (g x), here f = (sum.), g = (map2 (*) lst1)]
--> ((\x -> sum . x) (map2 (*) lst1)) lst2
--> (sum . (map2 (*) lst1)) lst2
--> sum ((map2 (*) lst1) lst2)
--> sum (map2 (*) lst1 lst2)
-}
-- Problem 5
naturalSum :: Integer
naturalSum = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 ==0]
-- naturalSum: 233168
-- Problem 6
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]
primes :: [Integer]
primes = sieve [2..]
resultSum :: Integer
resultSum = sum $ takeWhile (<10000) primes
-- resultSum: 5736396

-- Part C
-- Problem 1
{-
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

There is no good calling other functions in the definition of a function,
since the definition of other functions may change. Also, the code is
more concise and readable in this new version.
-}
-- Problem 2
{-
This definition has no problem in functionality; but it is time-consuming
and space-consuming. First of all, using length, head and tail function in
definition is not good, since they may change over time. More importantly,
when Haskell is determining the input pattern, it must run length function
for many times. When the input list is large, it wastes a lot of time and
space. A better way is to use pattern matching, see below.

largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max x (largest xs)
-}

-- Part D
-- Problem 1
{-
fib 3
--> fib (3 - 1) + fib (3 - 2)
--> fib 2 + fib (3 - 2)
--> (fib (2 - 1) + fib (2 - 2)) + fib (3 - 2)
--> (fib 1 + fib (2 - 2)) + fib (3 - 2)
--> (1 + fib (2 - 2)) + fib (3 - 2)
--> (1 + fib 0) + fib (3 - 2)
--> (1 + 0) + fib (3 - 2)
--> 1 + fib (3 - 2)
--> 1 + fib 1
--> 1 + 1
--> 2
-}
-- Problem 2
{-
fact 3
--> 3 * fact (3 - 1)
--> 3 * ((3 - 1) * fact ((3 - 1) - 1))
--> 3 * (2 * fact ((3 - 1) - 1)))
--> 3 * (2 * (((3 - 1) - 1) * fact (((3 - 1) - 1) -1)))
--> 3 * (2 * ((2 - 1) * fact (((3 - 1) - 1) -1)))
--> 3 * (2 * (1 * fact (((3 - 1) - 1) -1)))
--> 3 * (2 * (1 * ((((3 - 1) - 1) -1) * fact ((((3 - 1) - 1) -1) - 1))))
--> ...

Since we don't force pattern matching in the first line of the definition,
we end up in a dead loop keep doing fact n = n * fact (n - 1). A better
way is shown below:

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

In this way, the evaluation would be

fact 3
--> 3 * fact (3 - 1)
--> 3 * fact 2
--> 3 * (2 * fact (2 - 1))
--> 3 * (2 * fact 1)
--> 3 * (2 * (1 * fact (1 - 1)))
--> 3 * (2 * (1 * fact 0))
--> 3 * (2 * (1 * 1))
--> 3 * (2 * 1)
--> 3 * 2
--> 6
-}
-- Problem 3
{-
reverse [1, 2, 3]
--> iter [1, 2, 3] []
--> iter [2, 3] (1 : [])
--> iter [3] (2 : (1 : []))
--> iter [] (3 : (2 : (1 : [])))
--> (3 : (2 : (1 : [])))
--> 3 : 2 : 1 : []
--> [3, 2, 1]
[The above three representation of list is essentially identical]

Given a list with length of n, we first take out head element from the
list one by one until the list is empty, which takes n time. Then,
we combine the elements together into a list in 1 time. So, the total
time would be n.
-}
-- Problem 4
{-
reverse [1, 2, 3]
--> reverse [2, 3] ++ [1]
--> (reverse [3] ++ [2]) ++ [1]
--> ((reverse [] ++ [3]) ++ [2]) ++ [1]
--> (([] ++ [3]) ++ [2]) ++ [1]
--> ([3] ++ [2]) ++ [1]
--> (3 : ([] ++ [2])) ++ [1]
--> 3 : (([] ++ [2]) ++ [1])
--> 3 : ([2] ++ [1])
--> 3 : (2 : ([] ++ [1]))
--> 3 : (2 : [1])
--> 3 : 2 : [1]
--> [3, 2, 1]

In linear time, reverse [1, 2, 3] results in (([] ++ [3]) ++ [2]) ++ [1],
instead of [] ++ [3] ++ [2] ++ [1]. So, every time we need to append
a huge list to a single tail, and according to the definition of (++),
it takes O(length of huge list) time. Suppose we have a list with length
n in the form of (([] + [n]) ++ [n - 1]) ++ ..., we need about
O(n + (n - 1) + ... + 1) = O(n^2 + n) = O(n^2) time to complete this task.
So, the total running time of this reverse function is O(n^2).
-}
-- Problem 5
{-
head (isort [3, 1, 2, 5, 4])
--> head (insert 3 (isort [1, 2, 5, 4]))
--> head (insert 3 (insert 1 (isort[2, 5, 4])))
--> head (insert 3 (insert 1 (insert 2 (isort[5, 4]))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (isort[4])))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort[]))))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
--> head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
--> head (insert 3 (insert 1 (2 : (4 : insert 5 []))))
--> head (insert 3 (1 : (2 : (4 : insert 5 []))))
--> head (1 : (insert 3 (2 : (4 : insert 5 []))))
--> 1
-}
-- Problem 6.1
{-
foldr max 0 [1, 5, 3, -2, 4]
--> max 1 (foldr max 0 [5, 3, -2, 4])
--> max 1 (max 5 (foldr max 0 [3, -2, 4]))
--> max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
--> max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
--> max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
--> max 1 (max 5 (max 3 (max -2 (max 4 0))))
--> max 1 (max 5 (max 3 (max -2 4)))
--> max 1 (max 5 (max 3 4))
--> max 1 (max 5 4)
--> max 1 5
--> 5

foldl max 0 [1, 5, 3, -2, 4]
--> foldl max (max 0 1) [5, 3, -2, 4]
--> foldl max (max (max 0 1) 5) [3, -2, 4]
--> foldl max (max (max (max 0 1) 5) 3) [-2, 4]
--> foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
--> foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
--> max (max (max (max (max 0 1) 5) 3) -2) 4
--> max (max (max (max 1 5) 3) -2) 4
--> max (max (max 5 3) -2) 4
--> max (max 5 -2) 4
--> max 5 4
--> 5

The space complexity of foldr and foldl is the same, since both of them
need to store all elements in the list before calculating maximum. If
Haskell is not lazy, foldl would win the fight.
-}