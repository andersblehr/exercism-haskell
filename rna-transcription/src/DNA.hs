module DNA (toRNA) where

toRnaNucleotide :: Char -> Maybe Char
toRnaNucleotide 'G' = Just 'C'
toRnaNucleotide 'C' = Just 'G'
toRnaNucleotide 'T' = Just 'A'
toRnaNucleotide 'A' = Just 'U'
toRnaNucleotide _   = Nothing

toRNA :: String -> Maybe String
toRNA = mapM toRnaNucleotide

