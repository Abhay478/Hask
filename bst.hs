{-# LANGUAGE BlockArguments #-}
data Tree a = Tree a (Tree a) (Tree a)
            | Nil
            deriving (Show, Eq)

empty :: Tree a -> Bool
empty Nil = True
empty  _  = False

left :: Tree a -> Tree a
left (Tree _ l _) = l
left Nil = Nil

right :: Tree a -> Tree a
right (Tree _ _ r) = r
right Nil = Nil

root :: Num p => Tree p -> p
root (Tree x _ _) = x
root Nil = -1

search :: (Num a, Ord a) => Tree a -> a -> Tree a
search t n
  | root t == -1 = Nil
  | root t == n = t
  | root t < n = search (right t) n
  | otherwise = search (left t) n

-- insert :: (Num a, Ord a) => Tree a -> a -> Tree a
-- insert :: (Num a, Ord a) => Tree a -> a -> Tree a
-- insert t n 
--     | t == Nil = Tree n Nil Nil
--     | root t == n = t
--     | root t > n = 
--         Tree (root t) (insert (left t) n) (right t)
--     | root t < n = 
--         Tree (root t) (left t) (insert (right t) n)

insert :: Ord a => Tree a -> a -> Tree a
insert Nil n = Tree n Nil Nil
insert (Tree x l r) n
    | x == n = Tree x l r
    | x < n = Tree x l (insert r n)
    | otherwise = Tree x (insert l n) r

leftmost :: Tree a -> a
leftmost (Tree x Nil _) = x
leftmost t = leftmost $ left t

rightmost :: Tree a -> a
rightmost (Tree x _ Nil) = x
rightmost t = rightmost $ right t


removeTree :: Ord a => Tree a -> a -> Tree a
removeTree Nil n = Nil
removeTree (Tree x l r) n
    | x == n = Nil
    | x > n =
        Tree x (removeTree l n) r
    | otherwise =
        Tree x l (removeTree r n)

removeNode :: Ord a => Tree a -> a -> Tree a
removeNode (Tree x Nil Nil) n
    | x == n = Nil
    | x /= n = Tree x Nil Nil
removeNode (Tree x Nil r) n
    | x > n = Tree x Nil r
    | x == n = r
    | x < n = removeNode r n
removeNode (Tree x l Nil) n
    | x < n = Tree x l Nil
    | x == n = l
    | x < n = removeNode l n
removeNode (Tree x l r) n
    | x < n = Tree x l (removeNode r n)
    | x > n = Tree x (removeNode l n) r
    | x == n = Tree (rightmost l) (removeTree l (rightmost l)) r
removeNode Nil n = Nil


inorder :: Tree [a] -> [a]
inorder (Tree x l r) = 
    inorder l ++
    x ++
    inorder r
inorder Nil = []

main :: IO ()
main = do
    let t1 = Tree 4 (Tree 2 (Tree 1 Nil Nil) (Tree 3 Nil Nil)) (Tree 6 (Tree 5 Nil Nil) (Tree 7 Nil Nil))
    let t2 = Nil
    t2 <- return $ insert t2 3
    print $ show t1
    print $ show $ insert Nil 5
    print $ show t2

    -- print $ left t1
    print $ search t1 17
    print t2
    t1 <- return $ insert t1 17
    print t1
    print $ search t1 17
    t1 <- return $ removeNode t1 4
    print $ show t1
    t1 <- return $ removeTree t1 3
    print t1