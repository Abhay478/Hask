main = interact wordCount
    where wordCount input = show [ length (lines input), length (words input), length input] ++ "\n"