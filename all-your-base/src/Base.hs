module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase digits
    | inputBase  <= 1       = Left InvalidInputBase
    | outputBase <= 1       = Left InvalidOutputBase
    | (not . null) invalids = Left $ InvalidDigit (head invalids)
    | otherwise             = Right $ reverse (toOutput fromInput)
    where invalids  = filter (\d -> d < 0 || d >= inputBase) digits
          toOutput  = toOutputBase outputBase 
          fromInput = fromInputBase inputBase 0 (reverse digits)

fromInputBase :: Integral a => a -> a -> [a] -> a
fromInputBase _ _ []       = 0
fromInputBase base pos (d:ds) = d * base ^ pos + fromInputBase base (pos + 1) ds

toOutputBase :: Integral a => a -> a -> [a]
toOutputBase _ 0   = []
toOutputBase base n = digit : toOutputBase base (quot (n - digit) base)
    where digit = rem n base
