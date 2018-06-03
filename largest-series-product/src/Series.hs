module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | size == 0         = Right 1
    | size < 0          = Left InvalidSpan
    | size > nofDigits  = Left InvalidSpan
    | size == nofDigits = multiply digits
    | otherwise         = max <$> multiply (take size digits) <*> largestProduct size (tail digits)
    where nofDigits = length digits
          multiply [d]    = toInt d
          multiply (d:ds) = (*) <$> toInt d <*> multiply ds
          toInt d         = if isDigit d
                            then Right (read [d] :: Integer)
                            else Left (InvalidDigit d)
