
-- The partition function. This uses the first element of the list as the
-- partition, and moves the list elements to either side of the partition
-- (based on if they're lower / heigher than the chosen pivot element).
partition :: Ord a => [a] -> [a]
partition (pivot:xs) = concat [[y | y <- xs, y < pivot], [pivot], [y | y <- xs, y > pivot]]
