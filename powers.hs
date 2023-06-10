main :: IO ()
main = do
    buf <- getLine
    let len = (read buf :: Int)
    let q = [3^n | n <- [0..len]]
    print q
    let sums = scanl (+) 0 q
    -- let sums = scanl myAdd 0 q
    print sums

myAdd :: Num a => a -> a -> a
myAdd x y = x + y