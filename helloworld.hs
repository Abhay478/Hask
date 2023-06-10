module Main where
    main :: IO()
    main = do
        q <- getLine
        let q_int = (read q :: Int)
        print (q_int `div` 3)
        print (q_int `mod` 3)
        print (q_int `quot` 3)
        print (q_int `rem` 3)
        print "Hello, world."


