module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n $ iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]
