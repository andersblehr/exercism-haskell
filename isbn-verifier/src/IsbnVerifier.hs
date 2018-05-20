module IsbnVerifier (isbn) where

import Data.Char (isDigit)

isbn :: String -> Bool
isbn = checkChecksum . hasDigits . hasChecksum . hasLength . stripDashes
    where stripDashes      = filter (/= '-')
          hasLength s      = if length s == 10 then s else "-"
          hasChecksum s    = if isChecksum . head $ reverse s then s else "-"
          isChecksum d     = if isDigit d || d == 'X' then True else False
          hasDigits s      = if isAllDigits . tail $ reverse s then s else "-"
          isAllDigits      = foldl (\acc d -> acc && isDigit d) True
          checkChecksum    = (== 0) . flip mod 11 . foldl partSum 0 . zip [1..]
          partSum s (i, d) = s + i * (if isDigit d then read [d] :: Int else 10)
