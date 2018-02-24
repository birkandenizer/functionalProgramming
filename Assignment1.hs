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
