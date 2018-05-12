module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter anyMultiple [1..limit-1]
    where anyMultiple number = any (\factor -> rem number factor == 0) factors
