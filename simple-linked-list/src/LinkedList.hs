module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

import Data.Maybe (fromJust, isNothing)

data LinkedList a = LinkedList { datum' :: Maybe a
                               , next'  :: Maybe (LinkedList a)
                               } deriving (Eq, Show)

datum :: LinkedList a -> a
datum = fromJust . datum'

fromList :: [a] -> LinkedList a
fromList = foldr new nil

isNil :: LinkedList a -> Bool
isNil = isNothing . datum'

new :: a -> LinkedList a -> LinkedList a
new x list = LinkedList { datum'=Just x, next'=Just list }

next :: LinkedList a -> LinkedList a
next = fromJust . next'

nil :: LinkedList a
nil = LinkedList { datum'=Nothing, next'=Nothing }

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList list
    | isNil list = []
    | otherwise  =  datum list : toList (next list)
