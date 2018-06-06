module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n pascal where
    pascal = [1] : map next pascal
    next row = zipWith (+) (0 : row) (row ++ [0])