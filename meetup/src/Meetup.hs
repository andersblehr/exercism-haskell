module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Enum, Eq)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month
    | schedule == Last   = fromGregorian year month (last monthDays)
    | schedule == Teenth = fromGregorian year month (head (filter (>12) monthDays))
    | otherwise          = fromGregorian year month (monthDays !! fromEnum schedule)
    where monthDays =
            let (_, _, firstDay) = toWeekDate $ fromGregorian year month 1
                lastDay          = gregorianMonthLength year month
                dayDiff          = (fromEnum weekday + 1) - firstDay
            in filter (\d -> d > 0 && d <= lastDay) $ map (+ dayDiff) [1, 8 .. 29]
