module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = factorize 2
    where factorize factor n
            | n == 1            = []
            | rem n factor == 0 = factor : factorize factor (quot n factor)
            | otherwise         = factorize (factor + 1) n
