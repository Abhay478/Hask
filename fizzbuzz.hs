module Main where
import Control.Monad ( when )

fizzbuzz :: (Integral a, Show a) => a -> [Char]
fizzbuzz num | (num `mod` 15) == 0 = "Fizzbuzz"
             | (num `mod` 3) == 0 = "Fizz"
             | (num `mod` 5) == 0 = "Buzz"
             | otherwise = show num

main :: IO ()
main = do
    buf <- getLine
    let num = read buf :: Int
    let seq = [1..num]
    let fbseq = map fizzbuzz seq 
    print fbseq


    -- when (num `mod` 3 == 0) "Fizz" 
    -- when (num `mod` 5 == 0)  "Buzz" 
    -- when ((num `mod` 3) /= 0 && (num `mod` 5) /= 0) (show num)