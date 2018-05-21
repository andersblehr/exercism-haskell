module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)

scoreLetter :: Char -> Integer
scoreLetter letter
    | elem lower "aeioulnrst" =  1
    | elem lower "dg"         =  2
    | elem lower "bcmp"       =  3
    | elem lower "fhvwy"      =  4
    | elem lower "k"          =  5
    | elem lower "jx"         =  8
    | elem lower "qz"         = 10
    | otherwise               =  0
    where lower = toLower letter

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
