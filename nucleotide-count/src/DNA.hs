module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
    | validStrand xs = Right $ fromList [('A', countNucleotides 'A' xs)
                                        ,('C', countNucleotides 'C' xs)
                                        ,('G', countNucleotides 'G' xs)
                                        ,('T', countNucleotides 'T' xs)]
    | otherwise      = Left "Oops..."
    where validStrand                = all (\x -> elem x "ACGT")
          countNucleotides letter xs = length $ filter (== letter) xs
