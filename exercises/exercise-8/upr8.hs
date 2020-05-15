import Data.List (elemIndex)

netPairs :: [(Integer, Integer)]
netPairs = [(x, sum-x) | sum <- [2..], x <- [1..sum-1]]

pythTriples :: [(Integer, Integer, Integer)]
pythTriples = [(a,b,c) | c<-[5..],
                         let sht = floor (sqrt' (c^2 `div` 2)),
                         a<-[1..sht], 
                         let f = sqrt' (c^2 - a^2),
                         let b = floor f,
                         floor f == ceiling f]
  where sqrt' n = (sqrt (fromIntegral n))

-- compressHelper :: Eq a => [a] -> a -> Int
-- compressHelper lst elem
--   | null lst = 0
--   | head lst == elem = 1 + compressHelper (tail lst) elem
--   | otherwise = 0 


-- compress :: Eq a => [a] -> [(a, Int)]
-- compress [] = []
-- compress lst@(x:xs) = (x, n) : compress (drop n lst)
--   where n = compressHelper lst x

--има функция takeWhile и dropWhile, като ги комбинираме получаваме функцията span

countMyHead :: Eq a => [a] -> Int
countMyHead lst = length (takeWhile (== head lst) lst)

compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress lst = (head lst, count) : compress (drop count lst)
  where count = countMyHead lst

compress' [] = []
compress' lst = (head lst, length heads) : compress' rest
  where (heads, rest) = span (== head lst) lst

maxRepeated :: Eq a => [a] -> Int
--maxRepeated lst = maximum (map snd (compress lst))
maxRepeated = maximum . (map snd) . compress

makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs) = x : makeSet (filter (/=x) xs) -- /=x това е различно от x

-- makeSet lst = foldr (\el res -> if el `elem` res
--                                 then res
--                                 else el:res)
--                     []
--                     lst

histogramHelper :: Eq a => [a] -> a -> Int
histogramHelper lst elem
  | null lst = 0
  | head lst == elem = 1 + histogramHelper (tail lst) elem
  | otherwise = histogramHelper (tail lst) elem

histogramHelper е еквивалентен на count от ред 73
histogram :: [a] -> [(a, Int)]
histogram [] = []
histogram lst@(x:xs) = (x, n) : histogram (filter (/= x) xs)
  where n = histogramHelper lst x

histogram lst = [(el, count el lst) | el <- (makeSet lst)]
  where count x lst = length $ filter (==x) lst

histogram lst = map (\el -> (el, count el lst)) (makeSet lst)
  where count x lst = length $ filter (==x) lst