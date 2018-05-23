module SecretHandshake (handshake) where

reportoire = ["wink", "double blink", "close your eyes", "jump", "reverse"]

handshake :: Int -> [String]
handshake n = foldl addMove [] $ zip (bits n) reportoire
    where addMove moves (bit, move)
            | bit == 0          = moves
            | move == "reverse" = reverse moves
            | otherwise         = moves ++ [move]

bits :: Int -> [Int]
bits 0 = []
bits n = rem n 2 : bits (quot n 2)
          