module Lab3ab where
-- Part A: Basic exercises
-- Problem 1
{-
data Nat = Zero | Succ Nat
instance Eq Nat where
    Zero == Zero = True
    (Succ x) == (Succ y) = x == y
    _ == _ = False
    x /= y = not (x == y)
instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ x) = "Succ (" ++ show x ++ ")"
-}

-- Problem 2
data Nat = Zero | Succ Nat
    deriving (Eq, Show)

-- Problem 3
instance Ord Nat where
    Zero <= _ = True
    (Succ _) <= Zero = False
    (Succ x) <= (Succ y) = x <= y

{-
Derivation would work in this case, since Haskell would
order Nat by the constructor location, that is, Zero is 
the smallest, Succ Zero is the next smallest, and so on,
which is exactly the order we defined above.
-}

-- Problem 4
data SignedNat =
    Neg Nat | Pos Nat
    deriving (Show)

instance Eq SignedNat where
    (Pos x) == (Pos y) = x == y
    (Neg x) == (Neg y) = x == y
    (Pos Zero) == (Neg Zero) = True
    (Neg Zero) == (Pos Zero) = True
    _ == _ = False

instance Ord SignedNat where
    Neg _ <= Pos _ = True
    Neg x <= Neg y = x >= y
    Pos Zero <= Neg Zero = True
    Pos x <= Pos y = x <= y
    Pos _ <= Neg _ = False

{-
We could not use automatically-derived definitions for
Eq and Ord. For Eq, we know Neg Zero == Pos Zero = True,
but Haskell would not think in this way. For Ord, in
comparison among Negs, Haskell would consider Neg Zero
to be the smallest of Negs, but in fact it is the largest.
-}

-- Problem 5
addNat :: Nat -> Nat -> Nat
addNat Zero x = x
addNat x Zero = x
addNat (Succ x) (Succ y) = Succ (addNat x (Succ y))

{-
subNat function: represents subNat x y = x - y.
Here, in the subNat function, we only accept cases where
x >= y, otherwise we throw an error.
-}
subNat :: Nat -> Nat -> Nat
subNat (Succ x) (Succ y) | x >= y = subNat x y
subNat x Zero = x
subNat _ _ = error "1st argument too small"

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat _ Zero = Zero
mulNat (Succ x) y = addNat y (mulNat x y)

fromIntegerNat :: Integer -> Nat
fromIntegerNat 0 = Zero
fromIntegerNat x | x > 0 = Succ (fromIntegerNat (x - 1))
                 | otherwise = error "Negative!"

addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Neg x) (Neg y) = Neg (addNat x y)
addSignedNat (Pos x) (Pos y) = Pos (addNat x y)
addSignedNat (Neg x) (Pos y) | x <= y = Pos (subNat y x)
                             | otherwise = Neg (subNat x y)
addSignedNat (Pos x) (Neg y) | x <= y = Neg (subNat y x)
                             | otherwise = Pos (subNat x y)

subSignedNat :: SignedNat -> SignedNat -> SignedNat
subSignedNat (Pos x) (Neg y) = Pos (addNat x y)
subSignedNat (Neg x) (Pos y) = Neg (addNat x y)
subSignedNat (Pos x) (Pos y) | x <= y = Neg (subNat y x)
                             | otherwise = Pos (subNat x y)
subSignedNat (Neg x) (Neg y) | x <= y = Pos (subNat y x)
                             | otherwise = Neg (subNat x y)

mulSignedNat :: SignedNat -> SignedNat -> SignedNat
mulSignedNat (Neg x) (Neg y) = Pos (mulNat x y)
mulSignedNat (Neg x) (Pos y) = Neg (mulNat x y)
mulSignedNat (Pos x) (Neg y) = Neg (mulNat x y)
mulSignedNat (Pos x) (Pos y) = Pos (mulNat x y)

absSignedNat :: SignedNat -> SignedNat
absSignedNat (Pos x) = Pos x
absSignedNat (Neg x) = Pos x

signumSignedNat :: SignedNat -> SignedNat
signumSignedNat (Pos Zero) = Pos Zero
signumSignedNat (Neg Zero) = Pos Zero
signumSignedNat (Pos _) = Pos (Succ Zero)
signumSignedNat (Neg _) = Neg (Succ Zero)

fromIntegerSignedNat :: Integer -> SignedNat
fromIntegerSignedNat 0 = Pos Zero
fromIntegerSignedNat x | x > 0 = Pos (fromIntegerNat x)
                       | otherwise = Neg (fromIntegerNat (-x))

instance Num SignedNat where
    (+) = addSignedNat
    (-) = subSignedNat
    (*) = mulSignedNat
    negate x = (Pos Zero) - x
    abs = absSignedNat
    signum = signumSignedNat
    fromInteger = fromIntegerSignedNat

-- Problem 6
signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos Zero) = 0
signedNatToInteger (Neg Zero) = 0
signedNatToInteger (Pos (Succ x)) = 1 + signedNatToInteger (Pos x)
signedNatToInteger (Neg (Succ x)) = -1 + signedNatToInteger (Neg x)

-- Problem 7
{-
Well, the SignedNat datatype has both Pos Zero and Neg Zero,
and in fact they are the same thing.
To solve this, we can add another unary encoding for minus
Integers (for example, Pred for predecessor), with Pred Zero
representing -1, Pred (Pred Zero) for -2, and so on. In this
way, we only have one element representing 0.
The data type is defined as:

data UnaryInteger = 
    Pred UnaryInteger | Zero | Succ UnaryInteger

The problem for this data type is sign conversion (i.e, take
abs, multiplication of positive number and negative number, 
and so on). In order to convert the sign, we need extra O(n)
time complexity (where n is the absolute value of that number).
A convert function is like:

convertSign :: UnaryInteger -> UnaryInteger
convertSign Zero = Zero
convertSign (Pred x) = Succ (convertSign x)
convertSign (Succ x) = Pred (convertSign x)

Comparing with SignedNat data type, we see that we get rid of
redundancy in Zero, but increased operation complexity in sign
conversion.
-}

-- Problem 8
factorial :: (Num a, Ord a) => a -> a
factorial x | x < (fromInteger 0) = error "negative number!"
            | x == (fromInteger 0) = (fromInteger 1)
            | otherwise = x * (factorial (x - (fromInteger 1)))
-- Answer is Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

-- Part B: Operators and fixities
-- Problem 1
{-
1. This operator is non-associative;
(>#<) :: Integer -> Integer -> String
(>#<) x y | x > y = "First Player"
          | x == y = "Tie"
          | x < y = "Second Player"
infix

2. This operator can be either left- or right-associative.
(+|) :: Integer -> Integer -> Integer
(+|) x y = (x + y) 'mod' 10
infixl
Example: 1 +| 7 +| 6 = 1 +| (7 +| 6) = 1 +| 3 = 4
     or: 1 +| 7 +| 6 = (1 +| 7) +| 6 = 8 +| 6 = 4

3. This operator is left-associative.
(&<) :: [Integer] -> Integer -> [Integer]
(&<) x y = x ++ [y]
infixl
Example: [1] &< 2 &< 3 = ([1] &< 2) &< 3 
                       = [1, 2] &< 3
                       = [1, 2, 3]

4. This operator is right-associative
(>&&) :: Integer -> [Integer] -> [Integer]
(>&&) x y = [x, x] ++ y
infixr
Example: 1 >&& 2 >&& [3] = 1 >&& (2 >&& [3])
                         = 1 >&& [2, 2, 3]
                         = [1, 1, 2, 2, 3]
-}

-- Problem 2
{-
In order to pass the type check, the operator could be of
any associativity (infixl, infixr, and infix).
9 +# 9 +# 2 = 1         (infixl)
            = 2         (infixr)
            = error     (infix)
2 +# 9 +# 9 = 2         (infixl)
            = 1         (infixr)
            = error     (infix)
From the above examples we can see infixl and infixr give
different results on the same input, and it is hard to tell
which one is correct. So personally I think the associativity
should be non-associative in order to avoid misunderstanding
and misusage of this operation.
-}