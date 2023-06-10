module Main where

fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = do
    q <- getLine
    print (map fact [0..(read q :: Integer)])
    print (scanl (*) 1 [1..(read q :: Integer)])