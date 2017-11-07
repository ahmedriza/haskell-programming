--
-- Chapter 6 of Haskell Programming, Christopher Allen
--

k :: (a, b) -> a
k (x, y) = x

k1 = k ((4-1), 10)

k2 = k ("three", (1+2))

k3 = k (3, True)

---------- Case Exercises ---------------------

fc :: Ord t => t -> t -> t
fc x y = if (x > y) then x else y

fc' :: Ord t => t -> t -> t
fc' x y = case (x > y) of
  True -> x
  False -> y

ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' n = case (even n) of
  True -> n + 2
  False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

flip' :: (x -> y -> z) -> y -> x -> z
flip' f x y = f y x

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' = 
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = (flip dodgy) 2

abs' :: Integer -> Integer
abs' x
  | x < 0 = (-x)
  | otherwise = x

isRightTriangle :: (Num a, Eq a) => a -> a -> a -> String
isRightTriangle a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "NOT RIGHT"

avgGrade :: (Fractional t, Ord t) => t -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers :: (Ord a, Num a, Num b) => a -> b
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

f :: Floating a => a -> a
f x = sin x

g :: Num a => a -> a
g x = x + 1


