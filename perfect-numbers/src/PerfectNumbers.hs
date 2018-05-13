module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0                  = Nothing
    | n == 1 || factorSum < n = Just Deficient
    | factorSum == n          = Just Perfect
    | otherwise               = Just Abundant
    where factorSum = factors n

factors :: Int -> Int
factors n = sum $ filter (\x -> rem n x == 0) [1..div n 2]
