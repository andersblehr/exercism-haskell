module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode = snd . foldl (\acc c -> if isDigit c
    then (fst acc ++ [c], snd acc)
    else ([], snd acc ++ if length (fst acc) > 0
        then (take (read (fst acc) :: Int) $ repeat c)
        else [c])) ([], [])
    
encode :: String -> String
encode [] = []
encode text = serialize $ reverse $ doEncode [(head text, 0)] text
    where serialize crecs = concat $ map (\(c, count) -> if count == 1
            then [c]
            else show count ++ [c]) crecs

doEncode :: [(Char, Int)] -> String -> [(Char, Int)]
doEncode crecs (c : []) = encodeChar crecs c
doEncode crecs (c : cs) = doEncode (encodeChar crecs c) cs

encodeChar :: [(Char, Int)] -> Char -> [(Char, Int)]
encodeChar all@(crec : crecs) c = if c == current
    then ((c, succ count) : crecs)
    else ((c, 1) : all)
    where current = fst crec
          count = snd crec