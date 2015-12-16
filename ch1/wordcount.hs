-- file: wordcount.hs
-- real world haskell page 15


main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"