{-
CodeWars
Implement a function that accepts 3 integer values a, b, c.
The function should return true if a triangle can be built with the sides of given length and false in any other case.
(In this case, all triangles must have surface greater than 0 to be accepted).
-}

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c | all (> 0) [a, b, c] =
 all p help
  where
    p (a, b, c) = a + b > c
    help = [(a, b, c), (a, c, b), (b, c, a)]
isTriangle _ _ _ = False
