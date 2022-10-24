{-

CodeWars                                                                                                                                   
                                                                                                                                     
Implement the function unique_in_order which takes as argument a sequence and returns a list of items without any elements with the \
same value next to each other and preserving the original order of elements.                                                         
                                                                                                                                     
For example:                                                                                                                         
                                                                                                                                     
uniqueInOrder "AAAABBBCCDAABBB" == "ABCDAB"                                                                                          
uniqueInOrder "ABBCcAD"         == "ABCcAD"                                                                                          
uniqueInOrder [1,2,2,3,3]       == [1,2,3]                                                                                           
                                                                                                                                     
-}

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder [] = []
uniqueInOrder (x:xs)
  | xs == [] = [x]
  | x == head xs = uniqueInOrder xs
  | otherwise = x : (uniqueInOrder xs)
