main = return ()

maxInt :: [Int] -> Int
maxInt []     = error "Empty list"
maxInt [x]    = x
maxInt (x:xs) = max x (maxInt xs)

removeFst :: Int -> [Int] -> [Int]
removeFst m (x:xs) | x == m = xs
                   | otherwise = x : removeFst m xs
removeFst m [] = []
