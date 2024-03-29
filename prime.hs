main :: IO ()
main = return ()

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k ^ 2 > n = n
        | otherwise = ldf (k + 1) n

ld :: Integer ->  Integer
ld n = ldf 2 n

prime0 :: Integer -> Bool
prime0 n | n < 1 = error "Not a positive integer"
         | n == 2 = False
         | otherwise = ld n == n
