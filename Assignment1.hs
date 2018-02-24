dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = (d + t1 + k + t2 + t3 + 5*j) `mod` 7
  where
    m' = if m <= 2 then m+12 else m
    y' = if m <= 2 then y-1 else y
    t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
    k = y' `mod` 100
    t2 = floor (fromIntegral k / 4.0)
    t3 = floor (fromIntegral j / 4.0)
    j = y' `div` 100

sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end = 0
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
      where
        nextY = if m == 12 then y + 1 else y
        nextM = if m < 12 then m + 1 else 1
        rest = sundays' nextY nextM

sundays1tail :: Integer -> Integer -> Integer
sundays1tail start end = sundays' start 1 0
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m acc
      | y > end = 0
      | otherwise = if dayOfWeek y m 1 == 1 then restplus1 else rest
      where
        nextY = if m == 12 then y + 1 else y
        nextM = if m < 12 then m + 1 else 1
        rest = sundays' nextY nextM acc
        restplus1 = sundays' nextY nextM acc+1
