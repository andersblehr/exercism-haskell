module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
    | validStrand xs = Right $ fromList [('A', countNucleotides xs 'A')
                                        ,('C', countNucleotides xs 'C')
                                        ,('G', countNucleotides xs 'G')
                                        ,('T', countNucleotides xs 'T')]
    | otherwise      = Left "Oops..."
    where validStrand xs             = all (\x -> elem x "ACGT") xs
          countNucleotides xs letter = length $ filter (\x -> x == letter) xs
