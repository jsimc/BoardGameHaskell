module RoseTree (
  Rose (Node),
  foldRose,
  sumRose,
  sizeRose,
  heightRose,
  leavesCount,
  leaves,
  elemsOnDepth
) where

data Rose a = Node a [Rose a] deriving (Show)

instance Functor Rose where
  fmap f (Node val children) = Node (f val) (fmap (fmap f) children)


foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node val []) = f acc val
foldRose f acc (Node val children) = foldl (foldRose f) (f acc val) children

sumRose :: Num a => Rose a -> a
sumRose = foldRose (+) 0

sizeRose :: Rose a -> Int
sizeRose (Node _ children) = 1 + sum (map sizeRose children)

heightRose :: Rose a -> Int
heightRose (Node _ []) = 0
heightRose (Node _ children) = 1 + maximum (map heightRose children)

leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ children) = sum (map leavesCount children)

leaves :: Rose a -> [a]
leaves (Node val []) = [val]
leaves (Node val children) = concatMap leaves children

-- vraca vrednosti svih elemenata na odredjenoj dubini
elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth depth (Node val children)
    | depth == 0 = [val]
    | otherwise =  concatMap (elemsOnDepth (depth-1)) children
