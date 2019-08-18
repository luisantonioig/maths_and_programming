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
         | n == 1 = False
         | otherwise = ld n == n


min' :: Int -> Int -> Int
min' m n | m >= n = n
         | otherwise  = m

mnmInt :: [Int] -> Int
mnmInt []     = error "Empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min' x (mnmInt xs)

maxInt :: [Int] -> Int
maxInt []     = error "Empty list"
maxInt [x]    = x
maxInt (x:xs) = max x (maxInt xs)

removeFst :: Eq a => a -> [a] -> [a]
removeFst m (x:xs) | x == m = xs
                   | otherwise = x : removeFst m xs
removeFst m [] = []

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs))
  where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs =
  let
    m = mnmInt xs
  in
    m : (srtInts (removeFst m xs))

average :: [Int] -> Rational
average xs = toRational (sum' xs) / toRational (length' xs)

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

count :: Char -> String -> Int
count c (ch:s) | ch == c = 1 + count c s
               | otherwise = count c s
count c [] = 0

blowup :: String -> String
blowup [x] = [x]
blowup xs  = blowup (init xs) ++ (take (length xs) (repeat (last xs)))

minStr' s t | s <= t = s
            | otherwise = t

mnmStr :: [String] -> String
mnmStr []     = error "Empty list"
mnmStr [x]    = x
mnmStr (x:xs) = minStr' x (mnmStr xs)

srtStrs' :: [String] -> [String]
srtStrs' [] = []
srtStrs' xs =
  let
    m = mnmStr xs
  in
    m : (srtStrs' (removeFst m xs))
prefix :: String -> String -> Bool
prefix [] ys         = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring xs (y:ys)  | prefix xs (y:ys) = True
                     | substring xs ys = True
                     | otherwise = False
substring xs [] = False

factors :: Integer -> [Integer]
factors n | n < 1 = error "Argument not positive"
          | n == 1 = []
          | otherwise = p : factors (div n p)
  where p = ld n

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map f xs

lengths :: [[a]] -> [Int]
lengths xs = map length xs

sumlengths :: [[a]] -> Int
sumlengths xs = sum (map length xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs

primes0 :: [Integer]
primes0 = filter' prime0 [2..]
