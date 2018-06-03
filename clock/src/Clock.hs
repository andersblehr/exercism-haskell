module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock { getHour :: Int
                   , getMin  :: Int } deriving (Eq, Show)

instance Num Clock where
    c1 + c2       = fromHourMin (getHour c1 + getHour c2) (getMin c1 + getMin c2)
    fromInteger n = Clock (quot (fromIntegral n) 60) (rem (fromIntegral n) 60)
    negate clock  = Clock (23 - getHour clock) (60 - getMin clock)

clockHour :: Clock -> Int
clockHour = getHour

clockMin :: Clock -> Int
clockMin = getMin

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (rem (quot minutes 60) 24) (rem minutes 60)
    where minutes = let signedMins = 60 * hour + min
                    in rem signedMins 1440 + if signedMins < 0 then 1440 else 0
          
toString :: Clock -> String
toString clock = printf "%02d" (getHour clock) ++ ":" ++ printf "%02d" (getMin clock)
