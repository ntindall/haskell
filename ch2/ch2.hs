-- ch2
-- p 29

myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)


lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne l = case (length l) of
  1 -> Just (head l)
  _ -> Just (head (drop (length l - 2) l))