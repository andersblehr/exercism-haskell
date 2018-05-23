module SecretHandshake (handshake) where

import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (fromMaybe)

reportoire = Map.fromList [ (    1, "wink"           )
                          , (   10, "double blink"   )
                          , (  100, "close your eyes")
                          , ( 1000, "jump"           )
                          , (10000, "reverse"        )
                          ]

handshake :: Int -> [String]
handshake n = foldl addMove [] (bits n 1)
    where addMove moves bit
            | null nextMove         = moves
            | nextMove == "reverse" = reverse moves
            | otherwise             = moves ++ [nextMove]
            where nextMove = fromMaybe [] $ Map.lookup bit reportoire

bits :: Int -> Int -> [Int]
bits 0 _ = []
bits n factor = factor * rem n 2 : bits (quot n 2) (factor * 10)
          