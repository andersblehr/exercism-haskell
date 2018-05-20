module Phone (number) where

import Data.Char (isDigit, isLetter)

number :: String -> Maybe String
number cs
    | containsLetters                           = Nothing
    | length digits == 11 && head digits == '1' = number (tail digits)
    | length digits == 10 && validDigits        = Just digits
    | otherwise                                 = Nothing
    where
        containsLetters = foldl (\acc c -> acc || isLetter c) False cs
        digits          = filter isDigit cs
        validDigits     = (not $ elem (head digits) ['0', '1']) &&
                          (not $ elem (digits !! 3) ['0', '1'])
