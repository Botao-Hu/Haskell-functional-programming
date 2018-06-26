module Lab4a where

import Data.Char

-- Part A
-- Problem 1.1
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = 
    putChar c >> myPutStrLn cs

-- Problem 1.2
-- The "do" here is redundant
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- Problem 1.3
-- Way I: simple desugaring
{-
greet2 :: IO ()
greet2 = putStr "Enter your name: " >>
         getLine >>= 
         \name -> (putStr "Hello, " >>
                   putStr name >>
                   putStrLn "!")
-}
-- Way II: complex desugaring
{-
greet2 :: IO ()
greet2 = putStr "Enter your name: " >>
         getLine >>= 
         \name -> case name of
            _ -> (putStr "Hello, " >>
                  putStr name >>
                  putStrLn "!")
-}
{-
The complex desugaring behaves the same as the simple desugaring
version, since there is no "non-exhaustive patterns / pattern
matching failure" error in the expression. For any output from
getLine, we can always run the rest of the function correctly, 
-}

-- Problem 1.4
-- Way I: simple desugaring
{-
greet3 :: IO ()
greet3 = putStr "Enter your name: " >>
         getLine >>= 
         \(n:ns) -> 
            let name = toUpper n : ns in
                putStr "Hello, " >>
                putStr name >>
                putStrLn "!"
-}
-- Way II: complex desugaring
{-
greet3 :: IO ()
greet3 = putStr "Enter your name: " >>
         getLine >>= 
         \y -> case y of
            (n:ns) -> 
                let name = toUpper n : ns in
                    putStr "Hello, " >>
                    putStr name >>
                    putStrLn "!"
            _ -> fail "Pattern match failure in do expression"
-}
{-
Here, if we type nothing to getLine, the simple desugaring
function would report Non-exhaustive patterns in lamba, which
is actually saying pattern match error, but the message is hard
to understand. In complex desugaring, however, we have already
taken this case into consideration and pass this to the fail 
expression.
-}