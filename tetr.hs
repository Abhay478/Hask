
main :: IO ()
main = do
    buf <- getLine
    print $ map (\x -> (x, x^x)) [1..(read buf :: Integer)]