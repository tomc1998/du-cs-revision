import qualified Data.List as List

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

-- Partition using a custom pivot. The first parameter is an integer, which is
-- an index into the list pointing to the pivot to use. If this is out of
-- bounds of the given list, this function will throw.
partition_custom_pivot :: Ord a => Int -> [a] -> [a]
partition_custom_pivot pivot_ix xs = 
  -- Using list comprehensions concat 2 sublists of elements with the chosen
  -- pivot in between, where the first sublist is the list of elements in xs less
  -- than the pivot and the second sublist is the list of elements in xs greater
  -- than the pivot.
  concat [
    [y | y <- xs, y < pivot], 
    [pivot], 
    [y | y <- xs, y > pivot]
  ]
  where pivot = xs !! pivot_ix

