module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (fromMaybe)

scores = Map.fromList
    [
      ('a',  1)
    , ('b',  3)
    , ('c',  3)
    , ('d',  2)
    , ('e',  1)
    , ('f',  4)
    , ('g',  2)
    , ('h',  4)
    , ('i',  1)
    , ('j',  8)
    , ('k',  5)
    , ('l',  1)
    , ('m',  3)
    , ('n',  1)
    , ('o',  1)
    , ('p',  3)
    , ('q', 10)
    , ('r',  1)
    , ('s',  1)
    , ('t',  1)
    , ('u',  1)
    , ('v',  4)
    , ('w',  4)
    , ('x',  8)
    , ('y',  4)
    , ('z', 10)
    ]

scoreLetter :: Char -> Integer
scoreLetter letter = fromMaybe 0 $ Map.lookup (toLower letter) scores

scoreWord :: String -> Integer
scoreWord = foldl (\acc letter -> acc + scoreLetter letter) 0
