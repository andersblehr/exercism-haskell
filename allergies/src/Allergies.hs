module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum, Bounded)

allergies :: Int -> [Allergen]
allergies score = map fst $ filter (\(_, v) -> score .&. v > 0) allergens
    where allergens = zip [first .. last] [1, 2, 4, 8, 16, 32, 64, 128]
            where first = minBound :: Allergen
                  last  = maxBound :: Allergen

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = elem allergen $ allergies score
