elemAt :: Int -> [a] -> a
elemAt _ [] = error "Empty List!"
elemAt i (x:xs)  
   | i <= 0 = x
   | otherwise = elemAt (i-1) xs

count :: [a] -> Int
count [] = 0
count (x:xs) = 1 + count xs
