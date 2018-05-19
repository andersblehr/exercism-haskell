module Raindrops (convert) where

import qualified Data.Map as Map (Map, fromList, keys, lookup)
import Data.Maybe (fromJust)

conversionMap = Map.fromList [(3, "Pling"), (5, "Plang"), (7, "Plong")]

convert :: Int -> String
convert n
    | isRaindrop n = foldl (\acc factor -> acc ++ if rem n factor == 0
        then fromJust (Map.lookup factor conversionMap)
        else []
        ) [] (Map.keys conversionMap)
    | otherwise    = show n
    where isRaindrop n = foldl (\acc x -> acc || if rem n x == 0
            then True
            else False
            ) False (Map.keys conversionMap)
