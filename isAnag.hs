import Data.List 
main :: IO ()
main = do
    s1 <- getLine
    s2 <- getLine
    print (sort s1 == sort s2)