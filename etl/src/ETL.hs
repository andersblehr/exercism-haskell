module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, toList, fromList)

transform :: Map a String -> Map Char a
transform legacy = fromList [(toLower letter, score) | (score, letters) <- toList legacy, letter <- letters]
