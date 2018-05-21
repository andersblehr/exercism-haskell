module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n" $ map verse [99, 98..0]
    
verse :: Int -> String
verse n
    | n > 0     = bottles n ++ " of beer on the wall, " ++ bottles n ++ " of beer.\n\
                  \Take " ++ one n ++ " down and pass it around, " ++ bottles (n - 1) ++ " of beer on the wall.\n"
    | otherwise = "No more bottles of beer on the wall, no more bottles of beer.\n\
                  \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    where bottles n
            | n > 1     = show n ++ " bottles"
            | n == 1    = "1 bottle"
            | otherwise = "no more bottles"
          one n
            | n > 1     = "one"
            | otherwise = "it"
