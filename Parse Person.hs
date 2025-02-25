{- Stepik: https://stepik.org/lesson/5746/step/9?unit=1256


Реализуйте функцию parsePerson, которая разбирает строки вида 
"firstName = John\nlastName = Connor\nage = 30" 
и возвращает либо результат типа Person, либо ошибку типа Error.

  - Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, 
      каждая из которых имеет вид X = Y. Если входная строка не имеет указанный вид, 
      то функция должна возвращать ParsingError.
  - Если указаны не все поля, то возвращается IncompleteDataError.
  - Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
  - Если в строке присутствуют лишние поля, то они игнорируются.


-}

module Main where

import Data.Char (isAlpha, isNumber, isAlphaNum, isSpace)
import Debug.Trace (trace)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show, Eq)
data Person = Person { firstName :: String, lastName :: String, age :: Int }

instance Show Person where
  show :: Person -> String
  show (Person fName lName age) =
    "{ firstName = " ++ fName
    ++ ", lastName = " ++ lName
    ++ ", age = " ++ show age ++ " }"

instance Eq Person where
  (==) :: Person -> Person -> Bool
  (Person fName1 lName1 age1) == (Person fName2 lName2 age2) =
    fName1 == fName2 && lName1 == lName2 && age1 == age2

data TokenType = Name | Equal | Number | START 
  deriving Show
data Token = Token {
    content    :: String
  , tokenType  :: TokenType
  , span'      :: (Int, Int)
  }

instance Show Token where
  show :: Token -> String
  show (Token content tt (x, y)) =
    "{ content = \"" ++ content
    ++ "\", tokenType = " ++ show tt
    ++ ", (x, y) = (" ++ show x ++ ", " ++ show y ++ ") }" 

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

scan :: String -> [Token]
scan string = map (\t -> t {content = trim $ content t }) $ tail $ reverse $ help string (0, 0) False []
  where
    help :: String -> (Int, Int) -> Bool -> [Token] -> [Token]
    help []     _      _      tokens      = tokens
    help str    span   isPrev []          = help str span isPrev [Token "" START (0, 0)]
    help (c:cs) (x, y) isPrev toks@(t:ts) = case c of
      '='  -> help cs (x+1, y) False (Token "=" Equal (x+1, y) : toks)
      '\n' -> help cs (0, y+1) False  toks
      ' '  -> if isPrev then
                 help cs (x+1, y) isPrev (Token (content t++[c]) Name (span' t) : ts) else
                 help cs (x+1, y) isPrev toks
      _ | c == '\r' || c == '\b' -> help cs (x, y) False toks
      _ | isAlpha c || c == '.'  -> if isPrev then 
            help cs (x+1, y) isPrev (Token (content t++[c]) Name   (span' t) : ts) else
            help cs (x+1, y) True   (Token             [c]  Name   (x, y)    : toks)
      _ | isNumber c -> if isPrev then
            help cs (x+1, y) isPrev (Token (content t++[c]) Number (span' t) : ts) else
            help cs (x+1, y) True   (Token             [c]  Number (x, y)    : toks)
      _ | otherwise  -> help cs (x+1, y) isPrev toks

parse :: [Token] -> Person -> Either Error Person
parse [ ]        p  =  wrapRes p
parse [_]        p  =  wrapRes p
parse (x:eq:y:ts) p  =  case x of
  _ | not $ parseExpr x eq y       -> Left ParsingError
  _ | content     x == "firstName" -> parse ts (p { firstName =        content y })
  _ | content     x == "lastName"  -> parse ts (p { lastName  =        content y })
  _ | content     x == "age"       -> if parseAge y   then
        parse ts (p { age = read $ content y }) else
        Left (IncorrectDataError $ content y)
  _ | otherwise     -> wrapRes p
  where
    parseExpr :: Token -> Token -> Token -> Bool
    parseExpr x eq y =
      foldl (\b e -> b && isAlpha e) True (content x)
      && content eq == "="
      && foldl (\b e -> b && (isAlphaNum e || e == ' ') || e == '.') True (content y)

    parseAge :: Token -> Bool
    parseAge age = foldl (\b e -> b && isNumber e) True (content age) && (read $ content age :: Int) > 0

wrapRes :: Person -> Either Error Person
wrapRes p = case p of
      _ | firstName p == "" -> Left IncompleteDataError
      _ | lastName  p == "" -> Left IncompleteDataError
      _ | age       p == 0  -> Left IncompleteDataError
      _ | otherwise         -> Right p

parsePerson :: String -> Either Error Person
parsePerson str = do
  let s = checkStr str
  case s of
    Left err  -> Left err
    Right str -> parse (scan str) (Person "" "" 0)

checkStr :: String -> Either Error String
checkStr str = case str of
  _ | '\n' `notElem` str                              -> Left ParsingError
  _ | foldl (\b l -> b && (l == "")) True (lines str) -> Left ParsingError
  _ | length (lines str) < 3                          -> Left IncompleteDataError 
  _ | otherwise -> Right str

scanPerson :: Either Error Person -> String
scanPerson (Left err) = show err
scanPerson (Right  p) = show p 
                  
main :: IO ()
main = do
  print passedTests
  print failedTests

-- Tests below were given from comments section in Stepic task --

test0 = parsePerson "firstName = John\nlastName = Connor\nage = 28" == Right (Person "John" "Connor" 28)
test1 = parsePerson "firstNme = John\nlastName = Connor\nage = 28" == Left IncompleteDataError
test2 = parsePerson "firstName = John\nlstName = Connor\nage = 28" == Left IncompleteDataError
test3 = parsePerson "firstName = John\nlastName = Connor\ncage = 29" == Left IncompleteDataError
test4 = parsePerson "firstName \nlastName = Connor\nage = 29" == Left ParsingError
test5 = parsePerson "firstName = John\nlastName = Connor\nage = age" == Left (IncorrectDataError "age")
test6 = parsePerson "firstName = John\nlastName = Connor" == Left ParsingError -- Left IncompleteDataError
test7 = parsePerson "firstName = John B.\nlastName = Connor\nage = 123" == Right (Person "John B." "Connor" 123) 
test8 = parsePerson "firstName = John B.\nlastName = Connor X Y\nage = 123" == Right (Person "John B." "Connor X Y" 123) 
test9 = parsePerson "firstName = firstName\nlastName = lastName\nage = 123" == Right (Person "firstName" "lastName" 123)
test10 = parsePerson "firstName = lastName\nlastName = firstName\nage = 123" == Right (Person "lastName" "firstName" 123)
test11 = parsePerson "firstName = age\nlastName = age\nage = 1" == Right (Person "age" "age" 1)
test12 = parsePerson "firstName = Many  Spaces\nlastName = In   The  Name\nage = 1" == Right (Person "Many  Spaces" "In   The  Name" 1)

passedTests = filter (\p -> fst p) [(test0,0),(test1,1),(test2,2),(test3,3),(test4,4),(test5,5),(test6,6),(test7,7),(test8,8),(test9,9),(test10,10),(test11,11),(test12,12)]

failedTests = filter (\p -> not $ fst p) [(test0,0),(test1,1),(test2,2),(test3,3),(test4,4),(test5,5),(test6,6),(test7,7),(test8,8),(test9,9),(test10,10),(test11,11),(test12,12)]
