module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows x = foldl (\rows' _ -> rows' ++ [next (if null rows'
                                            then []
                                            else last rows')]) [] [1..x]

next :: [Integer] -> [Integer]
next []  = [1]
next row = map (aggregate row) [0..length row]

aggregate :: [Integer] -> Int -> Integer
aggregate _ 0     = 1
aggregate row pos = if pos < length row
                    then row !! (pos - 1) + row !! pos
                    else 1
