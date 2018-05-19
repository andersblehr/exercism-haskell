module Diamond (diamond) where

import Data.Char (ord)

diamond :: Char -> Maybe [String]
diamond char
    | elem char ['A'..'Z'] = Just $ diamondLines (zip chars offsets)
    | otherwise            = Nothing
    where chars   = ['A'..char] ++ reverse ['A'..pred char]
          offsets = map (\n -> abs $ center - n) [0..maxOffset]
              where maxOffset = length chars - 1
                    center    = div maxOffset 2

diamondLines :: [(Char, Int)] -> [String]
diamondLines charOffsets = map diamondLine charOffsets
    where maxOffset = length charOffsets - 1
          diamondLine charOffset = map putChar [0..maxOffset]
              where putChar n = if n == offset || n == maxOffset - offset then char else ' '
                    char      = fst charOffset
                    offset    = snd charOffset

