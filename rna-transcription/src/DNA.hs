module DNA (toRNA) where

toRnaNucleotide :: Char -> Char
toRnaNucleotide 'G' = 'C'
toRnaNucleotide 'C' = 'G'
toRnaNucleotide 'T' = 'A'
toRnaNucleotide 'A' = 'U'

toRNA :: String -> Maybe String
toRNA strand = 
    if all (flip elem "GCTA") strand then
        Just $ map toRnaNucleotide strand
    else
        Nothing

