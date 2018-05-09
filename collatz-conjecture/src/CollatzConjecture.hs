module CollatzConjecture (collatz) where

collatzSeries :: Integer -> [Integer]
collatzSeries 1 = [1]
collatzSeries n
    | even n = n : collatzSeries (n `div` 2)
    | odd n  = n : collatzSeries (3 * n + 1)

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | otherwise = Just $ subtract 1 $ toInteger . length $ collatzSeries n
