
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

