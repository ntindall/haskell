-- ch4.hs
-- ntindall

import System.Environment (getArgs)

interactWith function inputFile outputFile = do 
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where myFunction = fixLines
        mainWith function = do
          args <- getArgs
          case args of
              [input, output] -> interactWith function input output
              _ -> putStrLn "error: exactly, two arguments needed"


splitLines [] = [] 
splitLines cs =
    let (pre, suf) = break isLineTerminator cs 
    in pre : case suf of
        ('\r':'\n':rest) -> splitLines rest
        ('\r':rest)      -> splitLines rest
        ('\n':rest)      -> splitLines rest
        _                -> []
isLineTerminator c = c == '\r' || c == '\n'

-- break partitions a list into two parts. it takes a function as its first
-- parameter and return a Bool to indicate whether to break the list at that
-- point. The break function returns a pair, which consists of the sublist
-- consumed before the predicate returned true (the prefix) and the rest of the
-- list

-- i.e. break odd [2,4,5,6,8] = ([2,4],[5,6,8])

fixLines :: String -> String 
fixLines input = unlines (splitLines input)