module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise              = Just $ length $ filter different $ zip xs ys
    where different pair = fst pair /= snd pair
