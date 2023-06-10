main = do
    r <- getLine
    let rr = words r
    print $ myDrop (read $ head rr :: Int) (rr !! 1)
    print $ lastButOne r

myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop n xs | n == 0 || null xs = xs
            | otherwise = myDrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne s = reverse s !! 1