module Acronym (abbreviate) where

import Data.Char (isLetter, isUpper, isLower, isPunctuation, isSpace, toUpper)

isAcronymBoundary :: Char -> Char -> Bool
isAcronymBoundary x y
    | (isSpace x || isPunctuation x) && isLetter y = True
    | isLower x && isUpper y                       = True
    | otherwise                                    = False

acronym :: String -> String -> String
acronym acc (_:[])          = acc
acronym acc (x:y:xys)
    | acc == []             = acronym [toUpper x] (y:xys)
    | isAcronymBoundary x y = acronym (acc ++ [toUpper y]) xys
    | otherwise             = acronym acc (y:xys)

abbreviate :: String -> String
abbreviate = acronym []
