module RedBlackTree where

-- A color is either red or black.
data Color = Red | Black
  deriving Show

-- A red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees, and a value of type a.
-- the first tree is left tree, the second one is right tree.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
  deriving Show

-- Part A
-- Problem 1
member :: Ord a => a -> Tree a -> Bool
member x (Node _ left y right) 
    | x == y = True
    | x > y = member x right
    | x < y = member x left
member _ _ = False

-- Problem 2
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ left x right) = 
    toList left ++ [x] ++ toList right

-- Problem 3
insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t) 
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right) 
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- invariants 2 and 3.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t = 
      Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t) -- case 1
    balance Black (Node Red l1 e1 (Node Red l2 e2 r2)) e t = 
      Node Red (Node Black l1 e1 l2) e2 (Node Black r2 e t) -- case 2
    balance Black l1 e1 (Node Red (Node Red l2 e2 r2) e t) =
      Node Red (Node Black l1 e1 l2) e2 (Node Black r2 e t) -- case 3
    balance Black l1 e1 (Node Red l2 e2 (Node Red tl e tr)) = 
      Node Red (Node Black l1 e1 l2) e2 (Node Black tl e tr) -- case 4
    balance color l e r = Node color l e r  -- no balancing needed

-- Problem 4
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

-- Problem 5
minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ left _ right) = 1 + min (minDepth left) (minDepth right)

maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ left _ right) = 1 + max (maxDepth left) (maxDepth right)

-- Problem 6
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 (Node _ left x right)
    | (cmpl left x) && (cmpr right x) 
        = (testInvariant1 left) && (testInvariant1 right)
    | otherwise = False
    where
        -- cmpl: compare a tree (assume left) with a value.
        cmpl :: Ord a => Tree a -> a -> Bool
        cmpl Leaf _ = True
        cmpl (Node _ _ y _) x = (y < x)
        -- cmpr: compare a tree (assume right) with a value.
        cmpr :: Ord a => Tree a -> a -> Bool
        cmpr Leaf _ = True
        cmpr (Node _ _ y _) x = (y > x)

-- Problem 7
testInvariant2 :: Tree a -> Bool
testInvariant2 (Node Red (Node Red _ _ _) _ _) = False
testInvariant2 (Node Red _ _ (Node Red _ _ _)) = False
testInvariant2 Leaf = True
testInvariant2 (Node _ left _ right) = (testInvariant2 left) && (testInvariant2 right)

-- Problem 8
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
    where
        -- Given a tree, return a list of the count of black nodes on every path
        -- from the root of the tree to a leaf.
        leafCounts :: Tree a -> Int -> [Int]
        leafCounts Leaf n = [n]
        leafCounts (Node Black left _ right) n = 
            (leafCounts left (n + 1)) ++ (leafCounts right (n + 1))
        leafCounts (Node Red left _ right) n = 
            (leafCounts left n) ++ (leafCounts right n)

        -- Return True if all the elements of a list are equal.
        allEqual :: Ord a => [a] -> Bool
        allEqual [] = True
        allEqual [_] = True
        allEqual (x:r@(y:_)) | x == y = allEqual r
                             | otherwise = False

-- Part B
-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList

-- Problem 1
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset s1 s2 = all (\x -> member x s2) (toList s1)

-- Problem 2
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet x y = (isSubset x y) && (isSubset y x)

-- Problem 3
union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = foldr (\x r -> insert x r) s1 (toList s2)

-- Problem 4
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = foldr (\x r -> if (member x s1) then insert x r else r) empty (toList s2)

-- Problem 5
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\x r -> if not (member x s2) then insert x r else r) empty (toList s1)