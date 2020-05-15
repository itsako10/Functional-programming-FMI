import Data.List (sort)

--това Анди го написа за 1-ва задача от контролното
kol :: [Int]
kol = 1:2:2:g 1 [2]
  where g x (1:q) = x:g (3-x) (q ++ [x])
        g x (2:q) =  x:x:g (3-x) (q ++ [x, x])

--това Анди го написа пак за 1-вата от контролното
countGroups ::[Int] -> [Int]
countGroups (x:y:xs)
  | x == y = 2 : countGroups xs
  | otherwise = 1 : countGroups (y:xs)

--извикваме го така: take 20 kol == take 20 (countGroups kol), за да проверим, че са ни ок нещата

--Анди го написа за зад2 от контролното за група А
type Segment = (Int, Int, Int) --брой тактове, размер, темпо
--typedef (Int, Int, Int) Segment
--using Segment = (Int, Int, Int)
type Piece = [Segment]

duration :: Fractional a => Piece -> a
duration p = sum [ broi1*razmer1/tempo1 | (broi, razmer, tempo) <- p,
                                      let broi1 = fromIntegral broi,
                                      let razmer1 = fromIntegral razmer,
                                      let tempo1 = fromIntegral tempo ]

--Анди го написа за зад2 на вариант Б
type Tact = Float 
type Party = [ Tact ]
type Partiture = [ Party ]

hasContaPunkt :: Partiture -> Bool
hasContaPunkt pt = or [ isContaPunkt x y | x<-pt, y<-pt]
  where isContaPunkt :: Party -> Party -> Bool
        isContaPunkt p1 p2 = all (\x -> x == first || x == 1/first) res
          where res = zipWith (/) p1 p2
                first = head res

setUnionHelper :: Ord a => [a] -> [a] -> [a]
setUnionHelper [] lst = lst 
--setUnionHelper lst [] = lst //тази проверка ми е излишна
setUnionHelper lst1 lst2
  | elem (head lst1) lst2 = setUnionHelper (tail lst1) lst2 
  | otherwise = (head lst1):setUnionHelper (tail lst1) lst2

setUnion :: Ord a => [a] -> [a] -> [a]
setUnion lst1 lst2 = sort(setUnionHelper lst1 lst2)

setUnion' :: Ord a => [a] -> [a] -> [a]
setUnion' lst1 lst2 = sort ([ x | x <- lst1, not(elem x lst2)] ++ lst2)

setUnion'' :: Ord a => [a] -> [a] -> [a]
setUnion'' lst1 lst2 = sort ((filter (\el -> not(elem el lst2)) lst1) ++ lst2)

setUnion''' :: Ord a => [a] -> [a] -> [a]
setUnion''' [] lst2 = lst2
setUnion''' lst1 [] = lst1 
setUnion''' lst1@(x:xs) lst2@(y:ys)
  | x == y    = x : (setUnion''' xs ys)
  | x < y     = x : (setUnion''' xs lst2)
  | otherwise = y : (setUnion''' lst1 ys)