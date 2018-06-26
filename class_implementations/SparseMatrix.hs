-- Part C: Miniproject - Sparse Matrices
-- Data type declaration
module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
    SM { bounds     :: (Integer, Integer),  -- number of rows, columns
         rowIndices :: S.Set Integer,       -- row indices with nonzeros
         colIndices :: S.Set Integer,       -- column indices with nonzeros
         vals       :: (M.Map (Integer, Integer) a) }  -- non-zero values
    deriving (Eq, Show)

-- Problem 1
sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
-- error handling
sparseMatrix elem (row, col) 
    | row < 1 || col < 1 = error "invalid bounds"
    | all (\((w1, w2), _) -> (w1 <= row && w2 <= col)) elem == False
        = error "invalid bounds"
    | all (\((w1, w2), _) -> (w1 >= 1 && w2 >= 1)) elem == False
        = error "invalid row/colunm input"
-- parse matrix
    | otherwise =
        let m = M.filter (\x -> x /= 0) (M.fromList elem) in
            SM { bounds = (row, col),
                 rowIndices = S.fromList (map (\(x, _) -> x) (M.keys m)),
                 colIndices = S.fromList (map (\(_, y) -> y) (M.keys m)),
                 vals = m }

-- Problem 2
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM { bounds = (x1, y1), 
            rowIndices = _, 
            colIndices = _,
            vals = valMap1 }) 
      (SM { bounds = (x2, y2),
            rowIndices = _,
            colIndices = _,
            vals = valMap2 })
      | x1 /= x2 || y1 /= y2 = error "matrix dimension not matched"
      | otherwise = 
            let m = M.filter (\x -> x /= 0) (M.unionWith (+) valMap1 valMap2) in
                SM { bounds = (x1, y1),
                     rowIndices = S.fromList (map (\(x, _) -> x) (M.keys m)),
                     colIndices = S.fromList (map (\(_, y) -> y) (M.keys m)),
                     vals = m }

-- Problem 3
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM { bounds = (x1, y1), 
               rowIndices = rowSet1, 
               colIndices = colSet1,
               vals = valMap1})
    = SM { bounds = (x1, y1),
           rowIndices = rowSet1,
           colIndices = colSet1,
           vals = M.map (\x -> 0 - x) valMap1 }

-- Problem 4
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM x y = addSM x (negateSM y)

-- Problem 5
{-
A helper function for multiplication of a single row and a single column.
It takes out each element from the row, and check if that column contains
correponding non-zero element. If yes, then multiply these two elements
and add to the sum; if not, leave this result as zero. After that, the
function goes down to the next element of the row.
By saying corresponding element, we mean coordinates (row1, col1) and 
(row2, col2) which satisfy col1 == row2. 
-}
mulRC :: (Eq a, Num a) => Integer -> [((Integer, Integer), a)]
                          -> (M.Map (Integer, Integer) a) -> a
mulRC c (((_, c1), v1):lst1) m2 = 
    (mulRC c lst1 m2) + v1 * (M.findWithDefault 0 (c1, c) m2)
mulRC _ [] _ = 0

{-
A map function which does multiplication of combinations of non-zero rows
and non-zero columns. The combination is represented by a list of index
pairs of row from the first matrix and column of the second matrix. The
function extracts an element of that list, and pass it into the mulRC
function, which returns a multiplication value with type a. Then this
function map this value with its key - (the index pair) and convert them
into a map type.
-}
mulMap :: (Eq a, Num a) => [(Integer, Integer)] -> (M.Map (Integer, Integer) a)
                                                -> (M.Map (Integer, Integer) a)
                                                -> (M.Map (Integer, Integer) a)
mulMap ((r, c):rest) valMap1 valMap2 = 
    let lst1 = M.toList (M.filterWithKey (\(x, _) _ -> (x == r)) valMap1) in
        M.union (M.fromList [((r, c), (mulRC c lst1 valMap2))]) 
                (mulMap rest valMap1 valMap2)
mulMap [] _ _ = M.empty

{-
The "real" mulSM does cartesian product of rowIndices of the first matrix
and colIndices of the second matrix, thus getting a combination of row and
column pairs that are non-zero. Then it passes the combination and two
valMaps into the mulMap function, which returns a result map. At last, the
function filters out zero values and convert the data into a SparseMatrix.
In the calculation, we only care about non-zero rows and non-zero columns,
and in multiplication of a single row and a single column, we also get rid
of zero values. In this way, we take the advantage of the sparsity and 
do a faster calculation.
-}
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM { bounds = (x1, y1), 
            rowIndices = r1, 
            colIndices = _,
            vals = valMap1 }) 
      (SM { bounds = (x2, y2),
            rowIndices = _,
            colIndices = c2,
            vals = valMap2 })
      | y1 /= x2 = error "matrix dimension not matched"
      | otherwise = 
            let m = M.filter (\x -> x /= 0) 
                             (mulMap (S.toList (S.cartesianProduct r1 c2))
                                    valMap1 valMap2) in
                SM { bounds = (x1, y2),
                     rowIndices = S.fromList (map (\(x, _) -> x) (M.keys m)),
                     colIndices = S.fromList (map (\(_, y) -> y) (M.keys m)),
                     vals = m }

-- Problem 6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM { bounds = (x1, y1), 
            rowIndices = _, 
            colIndices = _,
            vals = valMap1 }) (x, y)
    | x > x1 || y > y1 = error "reference out of bounds"
    | otherwise = M.findWithDefault 0 (x, y) valMap1

rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM { bounds = (x1, _), 
             rowIndices = _, 
             colIndices = _,
             vals = _ })
    = x1

colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM { bounds = (_, y1), 
             rowIndices = _, 
             colIndices = _,
             vals = _ })
    = y1

-- Problem 7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM

-- Problem 8
{-
It is because a Num type class must define functions including signum,
fromInteger, and we cannot get any intuition of a sign of a matrix, or
an Integer representation of a matrix. So it is impossible for us to
define such functions, which makes it impossible for SparseMatrix to
be an instance of Num type class.
-}
