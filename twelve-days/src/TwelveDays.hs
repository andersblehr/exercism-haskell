module TwelveDays (recite) where

import System.IO.Unsafe (unsafePerformIO)

recite :: Int -> Int -> [String]
recite start stop = take (stop - start + 1)
    . drop (start - 1)
    . filter (not . null)
    . lines
    . unsafePerformIO $ readFile "twelve-days.txt"
