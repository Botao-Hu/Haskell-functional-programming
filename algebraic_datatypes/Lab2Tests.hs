--
-- Lab2Tests.hs
--

{-# LANGUAGE TemplateHaskell #-}

import qualified RedBlackTree as R

import qualified Data.List as L
import Test.HUnit
import Test.QuickCheck

---------------------------------------------------------------------------
-- Sample trees (both valid and invalid) for testing.
---------------------------------------------------------------------------

-- t1, t2, etc. are all valid trees.

t1 :: R.Tree Int
t1 = R.Leaf

t2 :: R.Tree Int
t2 = R.Node R.Red R.Leaf 1 R.Leaf

t3 :: R.Tree Int
t3 = R.Node R.Black R.Leaf 2 R.Leaf

t4 :: R.Tree Int
t4 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

t5 :: R.Tree Int
t5 = R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

t6 :: R.Tree Int
t6 = R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)

t7 :: R.Tree Int
t7 = R.Node R.Black 
      (R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)) 
      4 
      (R.Node R.Black (R.Node R.Black R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 R.Leaf))

t8 :: R.Tree Int
t8 = R.Node R.Black 
      (R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)) 
      4 
      (R.Node R.Red (R.Node R.Black R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 R.Leaf))

t9 :: R.Tree Int
t9 = R.Node R.Black 
     (R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)) 
     4 
     (R.Node R.Black (R.Node R.Red R.Leaf 5 R.Leaf) 6 (R.Node R.Red R.Leaf 7 R.Leaf))

-- These are invalid trees.
-- The name prefix is a label.
-- "o" -- violates ordering constraint.
-- "c" -- violates color constraint.
-- "b" -- violates black count constraints.

ot1 :: R.Tree Int
ot1 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 1 (R.Node R.Black R.Leaf 2 R.Leaf)

ot2 :: R.Tree Int
ot2 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 2 R.Leaf)

ot3 :: R.Tree Int
ot3 = R.Node R.Black (R.Node R.Black R.Leaf 2 R.Leaf) 1 (R.Node R.Black R.Leaf 3 R.Leaf)

ot4 :: R.Tree Int
ot4 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 3 (R.Node R.Black R.Leaf 2 R.Leaf)

ot5 :: R.Tree Int
ot5 = R.Node R.Black 
      (R.Node R.Black (R.Node R.Black R.Leaf 3 R.Leaf) 2 (R.Node R.Black R.Leaf 4 R.Leaf)) 
      1 
      (R.Node R.Black (R.Node R.Black R.Leaf 6 R.Leaf) 5 (R.Node R.Black R.Leaf 7 R.Leaf))

cbt6 :: R.Tree Int
cbt6 = R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)

cbt7 :: R.Tree Int
cbt7 = R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

ct8 :: R.Tree Int
ct8 = R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)

cbt9 :: R.Tree Int
cbt9 = R.Node R.Black 
       (R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)) 
       4 
       (R.Node R.Red (R.Node R.Red R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 R.Leaf))

ct10 :: R.Tree Int
ct10 = R.Node R.Black 
       (R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)) 
       4 
       (R.Node R.Red (R.Node R.Red R.Leaf 5 R.Leaf) 6 (R.Node R.Red R.Leaf 7 R.Leaf))

---------------------------------------------------------------------------
-- Unit tests on red-black trees (non-randomized).
---------------------------------------------------------------------------

-- 1) Unit tests on valid trees.

validTreeUnitTest1a = TestCase $ assertBool "t1 should satisfy invariant 1" (R.testInvariant1 t1) 
validTreeUnitTest1b = TestCase $ assertBool "t1 should satisfy invariant 2" (R.testInvariant2 t1) 
validTreeUnitTest1c = TestCase $ assertBool "t1 should satisfy invariant 3" (R.testInvariant3 t1) 

validTreeUnitTest2a = TestCase $ assertBool "t2 should satisfy invariant 1" (R.testInvariant1 t2) 
validTreeUnitTest2b = TestCase $ assertBool "t2 should satisfy invariant 2" (R.testInvariant2 t2) 
validTreeUnitTest2c = TestCase $ assertBool "t2 should satisfy invariant 3" (R.testInvariant3 t2) 

validTreeUnitTest3a = TestCase $ assertBool "t3 should satisfy invariant 1" (R.testInvariant1 t3) 
validTreeUnitTest3b = TestCase $ assertBool "t3 should satisfy invariant 2" (R.testInvariant2 t3) 
validTreeUnitTest3c = TestCase $ assertBool "t3 should satisfy invariant 3" (R.testInvariant3 t3) 

validTreeUnitTest4a = TestCase $ assertBool "t4 should satisfy invariant 1" (R.testInvariant1 t4) 
validTreeUnitTest4b = TestCase $ assertBool "t4 should satisfy invariant 2" (R.testInvariant2 t4) 
validTreeUnitTest4c = TestCase $ assertBool "t4 should satisfy invariant 3" (R.testInvariant3 t4) 

validTreeUnitTest5a = TestCase $ assertBool "t5 should satisfy invariant 1" (R.testInvariant1 t5) 
validTreeUnitTest5b = TestCase $ assertBool "t5 should satisfy invariant 2" (R.testInvariant2 t5) 
validTreeUnitTest5c = TestCase $ assertBool "t5 should satisfy invariant 3" (R.testInvariant3 t5) 

validTreeUnitTest6a = TestCase $ assertBool "t6 should satisfy invariant 1" (R.testInvariant1 t6) 
validTreeUnitTest6b = TestCase $ assertBool "t6 should satisfy invariant 2" (R.testInvariant2 t6) 
validTreeUnitTest6c = TestCase $ assertBool "t6 should satisfy invariant 3" (R.testInvariant3 t6) 

validTreeUnitTest7a = TestCase $ assertBool "t7 should satisfy invariant 1" (R.testInvariant1 t7) 
validTreeUnitTest7b = TestCase $ assertBool "t7 should satisfy invariant 2" (R.testInvariant2 t7) 
validTreeUnitTest7c = TestCase $ assertBool "t7 should satisfy invariant 3" (R.testInvariant3 t7) 

validTreeUnitTest8a = TestCase $ assertBool "t8 should satisfy invariant 1" (R.testInvariant1 t8) 
validTreeUnitTest8b = TestCase $ assertBool "t8 should satisfy invariant 2" (R.testInvariant2 t8) 
validTreeUnitTest8c = TestCase $ assertBool "t8 should satisfy invariant 3" (R.testInvariant3 t8) 

validTreeUnitTest9a = TestCase $ assertBool "t9 should satisfy invariant 1" (R.testInvariant1 t9) 
validTreeUnitTest9b = TestCase $ assertBool "t9 should satisfy invariant 2" (R.testInvariant2 t9) 
validTreeUnitTest9c = TestCase $ assertBool "t9 should satisfy invariant 3" (R.testInvariant3 t9) 

validTreeUnitTests = TestList 
  [ TestLabel "1a" validTreeUnitTest1a
  , TestLabel "1b" validTreeUnitTest1b
  , TestLabel "1c" validTreeUnitTest1c

  , TestLabel "2a" validTreeUnitTest2a
  , TestLabel "2b" validTreeUnitTest2b
  , TestLabel "2c" validTreeUnitTest2c

  , TestLabel "3a" validTreeUnitTest3a
  , TestLabel "3b" validTreeUnitTest3b
  , TestLabel "3c" validTreeUnitTest3c

  , TestLabel "4a" validTreeUnitTest4a
  , TestLabel "4b" validTreeUnitTest4b
  , TestLabel "4c" validTreeUnitTest4c

  , TestLabel "5a" validTreeUnitTest5a
  , TestLabel "5b" validTreeUnitTest5b
  , TestLabel "5c" validTreeUnitTest5c

  , TestLabel "6a" validTreeUnitTest6a
  , TestLabel "6b" validTreeUnitTest6b
  , TestLabel "6c" validTreeUnitTest6c

  , TestLabel "7a" validTreeUnitTest7a
  , TestLabel "7b" validTreeUnitTest7b
  , TestLabel "7c" validTreeUnitTest7c

  , TestLabel "8a" validTreeUnitTest8a
  , TestLabel "8b" validTreeUnitTest8b
  , TestLabel "8c" validTreeUnitTest8c

  , TestLabel "9a" validTreeUnitTest9a
  , TestLabel "9b" validTreeUnitTest9b
  , TestLabel "9c" validTreeUnitTest9c

  ]

-- 2) Unit tests on invalid trees.

invalidTreeUnitTest1 = TestCase $ 
  assertBool "ot1 should fail to satisfy invariant 1" (not (R.testInvariant1 ot1))
invalidTreeUnitTest2 = TestCase $ 
  assertBool "ot2 should fail to satisfy invariant 1" (not (R.testInvariant1 ot2))
invalidTreeUnitTest3 = TestCase $ 
  assertBool "ot3 should fail to satisfy invariant 1" (not (R.testInvariant1 ot3))
invalidTreeUnitTest4 = TestCase $ 
  assertBool "ot4 should fail to satisfy invariant 1" (not (R.testInvariant1 ot4))
invalidTreeUnitTest5 = TestCase $ 
  assertBool "ot5 should fail to satisfy invariant 1" (not (R.testInvariant1 ot5))

invalidTreeUnitTest6a = TestCase $ 
  assertBool "cbt6 should fail to satisfy invariant 2" (not (R.testInvariant2 cbt6))
invalidTreeUnitTest6b = TestCase $ 
  assertBool "cbt6 should fail to satisfy invariant 3" (not (R.testInvariant3 cbt6))
invalidTreeUnitTest7a = TestCase $ 
  assertBool "cbt7 should fail to satisfy invariant 2" (not (R.testInvariant2 cbt7))
invalidTreeUnitTest7b = TestCase $ 
  assertBool "cbt7 should fail to satisfy invariant 3" (not (R.testInvariant3 cbt7))
invalidTreeUnitTest8 = TestCase $ 
  assertBool "ct8 should fail to satisfy invariant 2" (not (R.testInvariant2 ct8))
invalidTreeUnitTest9a = TestCase $ 
  assertBool "cbt9 should fail to satisfy invariant 2" (not (R.testInvariant2 cbt9))
invalidTreeUnitTest9b = TestCase $ 
  assertBool "cbt9 should fail to satisfy invariant 3" (not (R.testInvariant3 cbt9))
invalidTreeUnitTest10 = TestCase $ 
  assertBool "ct10 should fail to satisfy invariant 2" (not (R.testInvariant2 ct10))

invalidTreeUnitTests = TestList 
  [ TestLabel "i1"  invalidTreeUnitTest1
  , TestLabel "i2"  invalidTreeUnitTest2
  , TestLabel "i3"  invalidTreeUnitTest3
  , TestLabel "i4"  invalidTreeUnitTest4
  , TestLabel "i5"  invalidTreeUnitTest5
  , TestLabel "i6a" invalidTreeUnitTest6a
  , TestLabel "i6b" invalidTreeUnitTest6b
  , TestLabel "i7a" invalidTreeUnitTest7a
  , TestLabel "i7b" invalidTreeUnitTest7b
  , TestLabel "i8"  invalidTreeUnitTest8
  , TestLabel "i9a" invalidTreeUnitTest9a
  , TestLabel "i9b" invalidTreeUnitTest9b
  , TestLabel "i10" invalidTreeUnitTest10
  ]

runRBUnitTests :: IO ()
runRBUnitTests = do
  putStrLn "validTreeUnitTests:\n"
  counts <- runTestTT validTreeUnitTests
  print counts
  putStrLn ""
  putStrLn "invalidTreeUnitTests:\n"
  counts <- runTestTT invalidTreeUnitTests
  print counts
  putStrLn ""

---------------------------------------------------------------------------
-- Randomized tests on red-black trees.
---------------------------------------------------------------------------

-- Test that all elements of a Tree constructed from a list
-- are in fact in the Tree.
prop_member :: [Int] -> Bool
prop_member lst =
  let t = R.fromList lst in
    all (\x -> R.member x t) lst

-- Test that converting a list to a tree and back is the same
-- as removing all duplicates from the list.
prop_list_tree_list :: [Int] -> Bool
prop_list_tree_list lst =
  let
    t = R.fromList lst
    lst' = R.toList t
    lst'' = L.nub lst
  in
    length lst' == length lst'' &&
    all (\x -> x `elem` lst') lst''

-- Test that the maximum depth of a red-black tree is no worst
-- than twice the minimum depth, where "depth" here means
-- the distance from the root to a leaf.
prop_depth :: [Int] -> Bool
prop_depth lst = 
  let 
    t = R.fromList lst
    minD = R.minDepth t
    maxD = R.maxDepth t
  in
    maxD >= minD && maxD <= 2 * minD

-- Test that tree invariants (ordering constraint) hold true for all trees.

prop_inv1 :: [Int] -> Bool
prop_inv1 lst = R.testInvariant1 (R.fromList lst)

prop_inv2 :: [Int] -> Bool
prop_inv2 lst = R.testInvariant2 (R.fromList lst)

prop_inv3 :: [Int] -> Bool
prop_inv3 lst = R.testInvariant3 (R.fromList lst)

---------------------------------------------------------------------------
-- Randomized tests on Sets.
---------------------------------------------------------------------------

-- Test that the union of an empty set with another set gives the other set.
prop_union_empty :: [Int] -> Bool
prop_union_empty lst =
  let s = R.toSet lst in
    R.eqSet (R.union R.empty s) s &&
    R.eqSet (R.union s R.empty) s

-- Test that (union s1 s2) == (union s2 s1).
prop_union_symmetric :: [Int] -> [Int] -> Bool
prop_union_symmetric lst1 lst2 =
  let 
    s1 = R.toSet lst1 
    s2 = R.toSet lst2
  in
    R.eqSet (R.union s1 s2) (R.union s2 s1)

-- Test that all elements of s1 and s2 are in (union s1 s2).
-- Test that all elements of (union s1 s2) are in either s1 or s2.
prop_union_complete :: [Int] -> [Int] -> Bool
prop_union_complete lst1 lst2 =
  let 
    s1 = R.toSet lst1 
    s2 = R.toSet lst2
    u  = R.union s1 s2
    ulist = R.toList u
  in
    all (\x -> R.member x s1 || R.member x s2) ulist &&
    all (\x -> R.member x u) lst1 &&
    all (\x -> R.member x u) lst2

-- Test that the intersection of an empty set with another set gives the empty set.
prop_intersection_empty :: [Int] -> Bool
prop_intersection_empty lst =
  let s = R.toSet lst in
    R.eqSet (R.intersection R.empty s) R.empty &&
    R.eqSet (R.intersection s R.empty) R.empty

-- Test that (intersection s1 s2) == (intersection s2 s1).
prop_intersection_symmetric :: [Int] -> [Int] -> Bool
prop_intersection_symmetric lst1 lst2 =
  let 
    s1 = R.toSet lst1 
    s2 = R.toSet lst2
  in
    R.eqSet (R.intersection s1 s2) (R.intersection s2 s1)

-- Test that all elements of (intersection s1 s2) are in both s1 and s2.
-- Test that all elements which are in both s1 and s2 are in (intersection s1 s2).
prop_intersection_complete :: [Int] -> [Int] -> Bool
prop_intersection_complete lst1 lst2 =
  let 
    s1 = R.toSet lst1 
    s2 = R.toSet lst2
    i  = R.intersection s1 s2
    ilist = R.toList i
  in
    all (\x -> R.member x s1 && R.member x s2) ilist &&
    all (\x -> if x `elem` lst2 then R.member x i else not (R.member x i)) lst1 &&
    all (\x -> if x `elem` lst1 then R.member x i else not (R.member x i)) lst2

-- Test that the difference of a set and the empty set is the first set.
-- Test that the difference of an empty set and a set is the empty set.
prop_difference_empty :: [Int] -> Bool
prop_difference_empty lst =
  let s = R.toSet lst in
    R.eqSet (R.difference R.empty s) R.empty &&
    R.eqSet (R.difference s R.empty) s

-- Test that all elements of (difference s1 s2) are in s1 but not s2.
-- Test that all elements that are in s1 but not s2 are in (difference s1 s2).
prop_difference_complete :: [Int] -> [Int] -> Bool
prop_difference_complete lst1 lst2 =
  let 
    s1 = R.toSet lst1 
    s2 = R.toSet lst2
    i  = R.difference s1 s2
    ilist = R.toList i
  in
    all (\x -> R.member x s1 && not (R.member x s2)) ilist &&
    all (\x -> if x `notElem` lst2 then R.member x i else not (R.member x i)) lst1 &&
    all (\x -> not (R.member x i)) lst2

-- Run all the quickcheck tests.
-- This uses Template Haskell; see
-- https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-All.html
-- for more details.
return []
allRandomizedTests = $quickCheckAll

runRandomizedTests :: IO ()
runRandomizedTests = do
  success <- allRandomizedTests
  if success
     then putStrLn "ALL RANDOMIZED TESTS PASSED!\n"
     else putStrLn "ERROR: SOME TESTS FAILED!\n"
  putStrLn ""

-- Run all the tests.
main :: IO ()
main = do
  putStrLn "Running randomized QuickCheck tests...\n"
  runRandomizedTests
  putStrLn "Running HUnit tests...\n"
  runRBUnitTests

