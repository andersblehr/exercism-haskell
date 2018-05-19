module School (School, add, empty, grade, sorted) where

import Data.List (insert)
import qualified Data.Map as Map (fromList, lookup)

type School = [(Int, [String])]

add :: Int -> String -> School -> School
add studentGrade student [] = [(studentGrade, [student])]
add studentGrade student allRecords@(record:records)
    | studentGrade == grade = ((grade, insert student students) : records)
    | studentGrade  < grade = ((studentGrade, [student]) : allRecords)
    | otherwise             = (record : add studentGrade student records)
    where grade    = fst record
          students = snd record

empty :: School
empty = []

grade :: Int -> School -> [String]
grade gradeNum school = case Map.lookup gradeNum $ Map.fromList school of
    Just students -> students
    Nothing       -> []

sorted :: School -> [(Int, [String])]
sorted school = school
