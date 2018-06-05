module House (rhyme) where

scenes = [ "the house that Jack built"
         , "the malt\nthat lay in"
         , "the rat\nthat ate"
         , "the cat\nthat killed"
         , "the dog\nthat worried"
         , "the cow with the crumpled horn\nthat tossed"
         , "the maiden all forlorn\nthat milked"
         , "the man all tattered and torn\nthat kissed"
         , "the priest all shaven and shorn\nthat married"
         , "the rooster that crowed in the morn\nthat woke"
         , "the farmer sowing his corn\nthat kept"
         , "the horse and the hound and the horn\nthat belonged to"
         ]

rhyme :: String
rhyme = concatMap (\n -> verse (take n scenes) (last - n)) [1..last]
    where last = length scenes

verse :: [String] -> Int -> String
verse scenes remaining = "This is " ++ describe scenes ++ if remaining > 0
                                                          then ".\n\n"
                                                          else ".\n"

describe :: [String] -> String
describe [scene]          = scene
describe (scene : scenes) = describe scenes ++ " " ++ scene
