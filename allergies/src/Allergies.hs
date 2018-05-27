module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum)

allergies :: Int -> [Allergen]
allergies score = map toEnum $ filter (testBit score) allergens
    where allergens = fromEnum <$> enumFrom Eggs

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = elem allergen $ allergies score
