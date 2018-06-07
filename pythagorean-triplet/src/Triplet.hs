module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = a^2 + b^2 == c^2

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c
    | isPythagorean (a, b, c) = (a, b, c)
    | isPythagorean (b, c, a) = (b, c, a)
    | otherwise               = (c, a, b)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets min max = [(a, b, c) | a <- [min..max]
                                         , b <- [min..max]
                                         , c <- [min..max]
                                         , a <= b
                                         , isPythagorean (a, b, c)]
