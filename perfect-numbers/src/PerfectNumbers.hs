module PerfectNumbers (classify, Classification(..)) where

import Data.List (nub)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0                  = Nothing
    | n == 1 || factorSum < n = Just Deficient
    | factorSum == n          = Just Perfect
    | otherwise               = Just Abundant
    where factorSum = sum $ factors n [1] 2

factors :: Int -> [Int] -> Int -> [Int]
factors n found x
    | x > div n 2  = nub found
    | rem n x == 0 = factors n (x:div n x:found) (x + 1)
    | otherwise    = factors n found (x + 1)
