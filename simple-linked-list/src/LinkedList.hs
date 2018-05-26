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

data LinkedList a = Nil | LinkedList { datum' :: Maybe a
                                     , next'  :: Maybe (LinkedList a)
                                     } deriving (Eq, Show)

datum :: LinkedList a -> a
datum = fromJust . datum'

fromList :: [a] -> LinkedList a
fromList = foldr new Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new x list = LinkedList { datum'=Just x, next'=Just list }

next :: LinkedList a -> LinkedList a
next = fromJust . next'

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList Nil  = []
toList list = datum list : toList (next list)
