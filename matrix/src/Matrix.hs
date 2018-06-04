module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , Matrix.fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.List.Split (chunksOf)
import Data.Vector

newtype Matrix a = Matrix { getRows :: Vector (Vector a) } deriving (Eq, Show)

cols :: Matrix a -> Int
cols matrix = if vlength (getRows matrix) > 0 then vlength (row 0 matrix) else 0
    where vlength = Data.Vector.length

column :: Int -> Matrix a -> Vector a
column x = Data.Vector.map (! x) . getRows

flatten :: Matrix a -> Vector a
flatten = Data.Vector.foldl (Data.Vector.++) Data.Vector.empty . getRows

fromList :: [[a]] -> Matrix a
fromList = Matrix . Data.Vector.fromList . Prelude.map Data.Vector.fromList

fromString :: Read a => String -> Matrix a
fromString = Matrix.fromList . pmap (pmap read) . pmap words . lines
    where pmap = Prelude.map

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape dims = Matrix.fromList . ptake size . chunksOf rs . toList . flatten
    where ptake = Prelude.take
          size  = rs * cs
          rs    = fst dims
          cs    = snd dims

row :: Int -> Matrix a -> Vector a
row x matrix = getRows matrix ! x

rows :: Matrix a -> Int
rows = Data.Vector.length . getRows

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix.fromList $ Prelude.map getCol [0 .. cols matrix - 1]
    where getCol col = toList $ column col matrix
