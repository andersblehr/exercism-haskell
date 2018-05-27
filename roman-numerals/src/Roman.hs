module Roman (numerals) where

import Control.Applicative

numerals :: Integer -> Maybe String
numerals n
    | n >= 4000 = Nothing
    | n >= 1000 =       (nums n ('M', '-', '-') ++) <$> numerals (n - 1000 * n')
    | n >=  100 =       (nums n ('C', 'D', 'M') ++) <$> numerals (n -  100 * n')
    | n >=   10 =       (nums n ('X', 'L', 'C') ++) <$> numerals (n -   10 * n')
    | otherwise = Just $ nums n ('I', 'V', 'X')
    where nums n (one, five, ten)
            | n' == 9   = [one, ten]
            | n' == 8   = [five, one, one, one]
            | n' == 7   = [five, one, one]
            | n' == 6   = [five, one]
            | n' == 5   = [five]
            | n' == 4   = [one, five]
            | n' == 3   = [one, one, one]
            | n' == 2   = [one, one]
            | n' == 1   = [one]
            | otherwise = []
          n'
            | n >= 1000 = quot n 1000
            | n >=  100 = quot n  100
            | n >=   10 = quot n   10
            | otherwise = n
          