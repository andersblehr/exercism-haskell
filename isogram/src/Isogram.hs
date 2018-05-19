module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram = (\letters -> nub letters == letters) . map toLower . filter isLetter
