module Bob (responseFor) where

import Data.Char (isLetter, isSpace, isUpper)

silence :: String -> Bool
silence chars = length chars == 0

question :: String -> Bool
question chars = last chars == '?'

yelling :: String -> Bool
yelling chars = all isUpper (filter isLetter chars) && any isLetter chars

responseFor :: String -> String
responseFor statement
    | silence chars                   = "Fine. Be that way!"
    | yelling chars && question chars = "Calm down, I know what I'm doing!"
    | yelling chars                   = "Whoa, chill out!"
    | question chars                  = "Sure."
    | otherwise                       = "Whatever."
    where chars = filter (not . isSpace) statement