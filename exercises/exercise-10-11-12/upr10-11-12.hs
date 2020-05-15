--на това упражнение почти всичко го писа Анди или ми помага, за да го напиша
--data Tree = Empty | Node Int Tree Tree deriving Show
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

t :: Tree Int
t = Node 5 (Node 2 (Node 10 Empty Empty)
                   (Node 4 (Node 3 Empty Empty)
                   Empty))
           (Node 3 (Node 1 Empty Empty)
                    Empty)

--можем да избягваме използването на такива фукнции,
--и да използваме само pattern matching
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty = 0
maxSumPath (Node val Empty Empty) = val --листо
maxSumPath (Node val Empty r) = val + maxSumPath r
maxSumPath (Node val l Empty) = val + maxSumPath l
maxSumPath (Node val l r) = val + max (maxSumPath l) (maxSumPath r)

-- isLeaf :: Tree -> Bool
-- isLeaf (Node val Empty Empty) = True
-- isLeaf Empty = False 
-- isLeaf (Node _ _ _) = False

isLeaf :: Tree a -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

prune :: Tree a -> Tree a
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node val l r) = (Node val (prune l) (prune r))

bloom :: Tree a -> Tree a
bloom Empty = Empty
bloom (Node val Empty Empty) = (Node val (Node val Empty Empty) (Node val Empty Empty))
bloom (Node val l r) = (Node val (bloom l) (bloom r))

rotateLeft, rotateRight :: Tree a -> Tree a
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node val l r) = Node (f val) (treeMap f l) (treeMap f r)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node val l r) = Node (f val) (fmap f l) (fmap f r)

data BST a = BEmpty | BNode a (BST a) (BST a)

instance Show a => Show (BST a) where 
  show t = show' 0 t
    where show' pad BEmpty = replicate pad ' ' ++ "#"
          show' pad (BNode val l r) = show' (pad+2) l
                                  ++ "\n"
                                  ++ replicate pad ' ' ++ show val
                                  ++ "\n"
                                  ++ show' (pad+2) r

bstInsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstInsert el BEmpty = BNode el BEmpty BEmpty
-- bstInsert el (BNode key BEmpty BEmpty)
--   | el <= key = BNode key (BNode el BEmpty BEmpty) BEmpty
--   | otherwise = BNode key BEmpty (BNode el BEmpty BEmpty) // това са излишни проверки
bstInsert el (BNode key l r)
  | el <= key = BNode key (bstInsert el l) r
  | otherwise = BNode key l (bstInsert el r)

data VTree a = VEmpty | VNode a [VTree a]
instance Functor VTree where
  fmap f VEmpty = VEmpty 
  fmap f (VNode val subtrees) = VNode (f val) (map (fmap f) subtrees)

bstSearch :: Ord a => a -> BST a -> Bool
bstSearch el BEmpty = False
bstSearch el (BNode key l r)
  | el == key = True 
  | el < key = bstSearch el l 
  | otherwise = bstSearch el r 

bstValues :: BST a -> [a]
bstValues BEmpty = []
bstValues (BNode key l r) = bstValues l ++ [key] ++ bstValues r

bstSize :: BST a -> Int
bstSize BEmpty = 0
bstSize (BNode _ l r) = 1 + bstSize l + bstSize r

--може да си направи и без foldr, можем да си обходим списъка елемент по елемент
bstFromList :: Ord a => [a] -> BST a
bstFromList lst = foldr bstInsert BEmpty lst

bstSort :: Ord a => [a] -> [a]
bstSort = bstValues . bstFromList

data Map k v = MEmpty | MNode k v (Map k v) (Map k v)
--data Maybe a = Nothing | Just a
mapSearch :: Ord k => k -> Map k v -> Maybe v 
mapSearch _ MEmpty = Nothing 
mapSearch k' ( MNode k v l r)
  | k' == k   = Just v 
  | k' < k    = mapSearch k' l 
  | otherwise = mapSearch k' r

data Direction = L | R deriving Show

bstPath :: Ord a => a -> BST a -> Maybe [Direction]
bstPath _ BEmpty = Nothing
bstPath x (BNode val l r)
  | x == val = Just []
  -- | x < val  = case bstPath x l of Nothing -> Nothing 
  --                                  Just path -> Just (L : path)
  | x < val   = fmap (L:) (bstPath x l) --може да се напише и така: | x < val =  (L:) <$> (bstPath x l)
  -- | otherwise  = case bstPath x r of Nothing -> Nothing
  --                                    Just path -> Just (R : path)
  | otherwise = (R:) <$> (bstPath x r)

-- Import Data.List (sort) -- за да можем да ползваме ф-я за сортиране
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (<x) xs) ++ [x] ++ quickSort (filter (>=x) xs)