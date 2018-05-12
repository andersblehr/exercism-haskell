module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n
    | n > 0 && n < 65 = Just $ 2 ^ (n - 1)
    | otherwise       = Nothing

total :: Integer
total = 2 ^ 64 - 1
