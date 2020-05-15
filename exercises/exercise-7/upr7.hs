import Data.List (elemIndex)

replicate' :: Integral a => a -> b -> [b]
replicate' 0 obj = []
replicate' n obj = obj : replicate' (n - 1) obj

--Andi go napisa
replicate2' :: Int -> a -> [a]
replicate2' n x = [x | i <- [1..n]]

minimum' :: Ord a => [a] -> a 
minimum' [] = error "Oops!!!1!"
minimum' (x : xs) = foldr min x xs

reverse' lst = foldr (\el res -> res ++ [el]) [] lst

--length' :: [a] -> Int
length' :: Integral a => [b] -> a --сигнатура
length' lst = foldr (\_ res -> res + 1) 0 lst --res е дължината на опашката

length2 lst = sum [1 | _ <- lst]

lenght3 lst = sum (map (const 1) lst)

all' :: (b -> Bool) -> [b] -> Bool
all' p lst = foldr (\el res -> p el && res) True lst

any' p lst = foldr (\el res -> p el || res) False lst

append' lst1 lst2 = foldr (:) lst2 lst1

makeSet' lst = foldr (\el res -> if el `elem` res then res else el:res) [] lst

divisorsCount :: Int -> Int
divisorsCount n = length [i | i <- [1..n], n `mod` i == 0]

prime :: Integer -> Bool
--prime n = (divisorsCount n) == 2 //бачка, но ще го направим с list comprehension
prime 1 = False
prime n = null [i | i <- [2..sqn], n `mod` i == 0]
  where sqn = floor (sqrt (fromIntegral n))

primes:: [Integer]
primes = filter prime [1..] --вика се с: take 20 primes

descartes :: [a] -> [b] -> [(a, b)]
descartes lst1 lst2 = [(a, b) | a <- lst1, b <- lst2]

natPairs' = descartes [1..] [1..] -- ако го направим така може никога да не завърши, например ако извикаме фукнцията с elemIndex (2,3) natPairs

natPairs = [(x, diag-x+2) | diag <- [0..], x <- [1..diag+1]] --извикай го с take 20 natPairs, за да видиш какво се случва