module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode [] = []
decode encoded = replicate count char ++ decode (drop skip encoded)
    where count   = if (not . null) digits then read digits else 1
          char    = encoded !! (skip - 1)
          skip    = length digits + 1
          digits  = takeWhile isDigit encoded

encode :: String -> String
encode [] = []
encode text = count ++ [char] ++ encode (drop skip text)
    where count = if skip > 1 then show skip else []
          skip  = length chars
          char  = head text
          chars = takeWhile (== char) text
