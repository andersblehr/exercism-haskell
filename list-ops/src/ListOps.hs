module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z xs = foldr' f z (reverse xs)
    where foldr' _ z []     = z
          foldr' f z (x:xs) = let z' = f x z in z' `seq` foldr' f z' xs

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

concat :: [[a]] -> [a]
concat = foldl' (++) []
