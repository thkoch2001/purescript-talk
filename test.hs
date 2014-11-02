{-# LANGUAGE FlexibleInstances #-}

import Data.Map (empty, unionWith, Map, singleton, elems)
import Test.QuickCheck
import Text.Show.Functions

partition :: Ord b => (a -> b) -> [a] -> Map b [a]
partition _ [] = empty
partition criterion (x:xs) = unionWith (++) one rest
  where
    one = singleton (criterion x) [x]
    rest = partition criterion xs

prop_retainsAllElements :: Ord b => (a -> b) -> [a] -> Bool
prop_retainsAllElements criterion set = l1 == l2
  where
    l1, l2 :: Int
    l1 = length set
    l2 = length $ concat $ elems (partition criterion set)
