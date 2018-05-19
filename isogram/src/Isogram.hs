module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram word = (length $ nub letters) == (length letters)
    where letters = map toLower $ filter isLetter word
