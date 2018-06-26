module State where

import Control.Monad
import Control.Monad.State
import Data.IORef

-- Part A
-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
    do b <- test 
       when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
    do s0 <- get
       when (test s0)
           (do modify (execState body)
               whileState test body)

-- Problem 1
factIO :: Integer -> IO Integer
factIO x 
    | x < 0 = error "invalid negative input" 
    | otherwise = 
        do n <- newIORef x
           ans <- newIORef 1
           whileIO
                (do n' <- readIORef n
                    return (n' > 0))
                (do n' <- readIORef n
                    ans' <- readIORef ans
                    writeIORef ans (ans' * n')
                    writeIORef n (n' - 1))
           readIORef ans

-- Problem 2
factState :: Integer -> Integer
factState x
    | x < 0 = error "invalid negative input" 
    | otherwise = evalState
        (do whileState (\(count, _) -> (count > 0))
                (do (count, ans) <- get
                    put (count - 1, ans * count))
            (_, ans) <- get
            return ans) (x, 1)

-- Problem 3
fibIO :: Integer -> IO Integer
fibIO x
    | x < 0 = error "invalid negative input" 
    | otherwise = 
        do count <- newIORef x
           n1 <- newIORef 0
           n2 <- newIORef 1
           whileIO
                (do count' <- readIORef count
                    return (count' > 0))
                (do n1' <- readIORef n1
                    n2' <- readIORef n2
                    count' <- readIORef count
                    writeIORef n1 n2'
                    writeIORef n2 (n1' + n2')
                    writeIORef count (count' - 1))
           readIORef n1

-- Problem 4
fibState :: Integer -> Integer
fibState x
    | x < 0 = error "invalid negative input" 
    | otherwise = evalState
        (do whileState (\(_, _, count) -> (count > 0))
                (do (n1, n2, count) <- get
                    put (n2, n1 + n2, count - 1))
            (ans, _, _) <- get
            return ans) (0, 1, x)

-- Part B
{-
1. derivation of >>=

Suppose we have two functions in the Reader r monad
f :: a -> Reader r b
g :: b -> Reader r c
and we compose these two and wish to get a function
h :: a -> Reader r c
To be illustrative, we write the non-monadic versions of f, g and h as
f' :: (a, r) -> b
g' :: (b, r) -> c
h' :: (a, r) -> c
and the definition for h' in terms of f' and g' is
h' :: (a, r) -> c
h' (x, rt) =
    let y = f' (x, rt)
    in g' (y, rt)

Then we write their curried versions
f'' :: a -> r -> b
g'' :: b -> r -> c
which lead to
f'' x rt = f' (x, rt)
g'' y rt = g' (y, rt)
and
f'' x = \rt -> f' (x, rt)
g'' y = \rt -> g' (y, rt)

If we wrap the right-hand sides of f'' and g'' in a Reader constructor,
we have the definitions of f and g in terms of f' and g'
f :: a -> Reader r b
f x = Reader (\rt -> f' (x, rt))
g :: b -> Reader r c
g y = Reader (\rt -> g' (y, rt))
and similarly for h,
h :: a -> Reader r c
h x = Reader (\rt -> h' (x, rt))

Go back to f, g, and h, we have
h = f >=> g
--> h x = f x >>= g
--> f x >>= g = h x
--> f x >>= g = Reader (\rt -> h' (x, rt))
[definition for h' in terms of f' and g' gives]
              = Reader (\rt -> let y = f' (x, rt)
                               in g' (y, rt))
[recall f x = Reader (\rt -> f' (x, rt))]
              = Reader (\rt -> let (Reader ff) = f x
                                   y = ff rt
                               in g' (y, rt))
[recall g y = Reader (\rt -> g' (y, rt))]
              = Reader (\rt -> let (Reader ff) = f x
                                   y = ff rt
                                   (Reader gg) = g y
                               in gg rt)

Substitute mv for f x to get
mv >>= g
    = Reader (\rt -> let (Reader ff) = mv
                         y = ff rt
                         (Reader gg) = g y
                     in gg rt))
Applying runReader to it gives
mv >>= g
    = Reader (\rt -> let y = runReader mv rt
                     in runReader (g y) rt)
The above two forms of definition of >>= operator are the same as
in the assignment, except slight difference in notations.

2. derivation of return

Return must be the identity function in the Monad, and the identity
function should be in the following form
id_reader :: (a, r) -> (a, r)
id_reader (x, rt) = (x, rt)

Or
id_reader' :: a -> (r -> (a, r))
id_reader' x = (\rt -> (x, rt))

which gives the monad form
id_reader_monad :: a -> Reader r a
id_reader_monad x = Reader (\rt -> (x, rt))

and this is also the return method
return :: a -> Reader r a
return x = Reader (\rt -> (x, rt))
which is the same as in the assignment.
-}