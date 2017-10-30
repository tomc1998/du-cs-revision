import qualified Data.List as List
import Data.Maybe (fromJust)

-- Recursively divide a list up into sublists of length 5.
divide_into_5s :: [a] -> [[a]]
divide_into_5s xs 
  | null xs = []
  | otherwise = concat [[(List.take 5 xs)], (divide_into_5s $ List.drop 5 xs)]

-- Find the median of a list.
median :: Ord a => [a] -> a
median xs = sorted !! ix
  where 
  sorted = List.sort xs
  ix = length xs `div` 2

-- Find multiple medians
medians :: Ord a => [[a]] -> [a]
medians (xs:xss) = concat [[median xs], medians xss]
medians [] = []

-- Find the medians of a list taken in 5s
medians_of_5s :: Ord a => [a] -> [a]
medians_of_5s = medians . divide_into_5s

-- Find the median of medians of a list taken in 5s
median_of_medians :: Ord a => [a] -> a
median_of_medians = median . medians_of_5s

-- Partition using a custom pivot. The first parameter is an integer, which is
-- an index into the list pointing to the pivot to use. If this is out of
-- bounds of the given list, this function will throw.
--
-- Returns a tuple of 2 lists, the first is the sublist partitioned before the pivote, and the
-- last is the sublist partitioned after the pivot.
partition_custom_pivot :: Ord a => Int -> [a] -> [a]
partition_custom_pivot pivot_ix xs = 
  concat [
    [y | y <- xs, y < pivot], 
    [pivot], 
    [y | y <- xs, y > pivot]
  ]
  where pivot = xs !! pivot_ix

-- Return the 'ith order statistic' of the given list.
select :: Ord a => Int -> [a] -> a
select i [] = error "Trying to run func select on an empty list"
select i xs 
  | i == new_pivot_ix = m
  | i < new_pivot_ix = select i (take sublist_len partitioned)
  | otherwise = select (i - new_pivot_ix) (drop sublist_len partitioned)
  where
  sublist_len = length xs `div` 2
  m = median_of_medians xs
  pivot_ix = fromJust $ List.elemIndex m xs
  partitioned = partition_custom_pivot pivot_ix xs
  new_pivot_ix = fromJust $ List.elemIndex m partitioned
