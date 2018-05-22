module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

type Robot = ((Integer, Integer), Bearing)

bearing :: Robot -> Bearing
bearing = snd

coordinates :: Robot -> (Integer, Integer)
coordinates = fst

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = (coordinates, direction)

simulate :: Robot -> String -> Robot
simulate = foldl step
    where step robot instruction
            | instruction == 'R' = mkRobot (turnRight direction) position
            | instruction == 'L' = mkRobot (turnLeft direction) position
            | instruction == 'A' = advance position
            where direction = bearing robot
                  position = coordinates robot
                  advance (x, y) = case direction of
                    North -> mkRobot direction (x, y + 1)
                    East  -> mkRobot direction (x + 1, y)
                    South -> mkRobot direction (x, y - 1)
                    West  -> mkRobot direction (x - 1, y)
                  
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North
