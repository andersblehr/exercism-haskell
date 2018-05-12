module SumOfMultiples (sumOfMultiples) where

multiple :: Integer -> Integer -> Bool
multiple number factor = rem number factor == 0

anyMultiple :: Integer -> [Integer] -> Bool
anyMultiple number = any (multiple number)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = sum $ filter (flip anyMultiple factors) [1..pred limit]
