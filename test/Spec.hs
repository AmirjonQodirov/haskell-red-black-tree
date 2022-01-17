{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

import Lib

main :: IO ()
main = do
  quickCheck prop_NoRedRed



--PROPERTY TEST--
prop_NoRedRed :: Tree Int -> Bool
prop_NoRedRed E = True
prop_NoRedRed (T R (T R _ _ _) _ _) = False
prop_NoRedRed (T R _ _ (T R _ _ _)) = False
prop_NoRedRed (T _ l x r) = (prop_NoRedRed l) && (prop_NoRedRed r)

prop_RootBlack :: Tree Int -> Bool
prop_RootBlack E = True
prop_RootBlack (T B _ _ _) = True

blackDepth :: Tree a -> Maybe Int
blackDepth (E) = Just(1)
blackDepth (T R l _ r) = case (blackDepth(l),blackDepth(r)) of
  (Just(n),Just(m)) -> if n == m then Just(n) else Nothing
  (_,_) -> Nothing
blackDepth (T B l _ r) = case (blackDepth(l),blackDepth(r)) of
  (Just(n),Just(m)) -> if n == m then Just(1+n) else Nothing
  (_,_) -> Nothing


prop_BlackBalanced :: Tree Int -> Bool
prop_BlackBalanced t =
 case blackDepth(t) of
  Just _ -> True
  Nothing -> False



--MODULE TEST--
insertTest :: TestTree
insertTest = testCase "Testing insert"
  (assertEqual "insert 5 to empty" (T B E 5 E) (Lib.put 5 Lib.empty))

containsTest :: TestTree
containsTest = testCase "Testing contains"
  (assertEqual "check 5 from empty" False (Lib.contains 5 Lib.empty))

containsTest2 :: TestTree
containsTest2 = testCase "Testing contains"
  (assertEqual "check 5 from T B E 5 E" True (Lib.contains 5 (T B E 5 E)))

deleteTest :: TestTree
deleteTest = testCase "Testing delete"
  (assertEqual "delete 5 from T B E 5 E" Lib.empty (Lib.delete 5 (T B E 5 E)))


