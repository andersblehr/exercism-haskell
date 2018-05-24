module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (isAnagram word)
    where isAnagram word template
            | lowerWord == lowerTemplate = False
            | otherwise                  = sort lowerWord == sort lowerTemplate
            where lowerWord     = map toLower word
                  lowerTemplate = map toLower template
