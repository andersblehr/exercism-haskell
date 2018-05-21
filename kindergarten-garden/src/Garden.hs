module Garden
    ( Plant (..)
    , garden
    , assign
    , lookupPlants
    ) where

import qualified Data.Map as Map (Map, fromList, fromListWith, lookup)
import Data.Maybe (fromJust)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = Map.Map String String

garden :: [String] -> String -> Garden
garden students plants = Map.fromListWith (flip (++)) $ concatMap (assign students) (lines plants)

assign :: [String] -> String -> [(String, String)]
assign (s : _) [p1, p2] = [(s, [p1, p2])]
assign (s : students) (p1 : p2 : plants) = (s, [p1, p2]) : assign students plants

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = map plant $ fromJust $ Map.lookup student garden
    where plant 'C' = Clover
          plant 'G' = Grass
          plant 'R' = Radishes
          plant 'V' = Violets
