-- Variables
let x = 42
let y = 2

-- List
words = ["hi", "hello", "howdy"]

-- Function
logThenReturn t = do
  (putStrLn (t))
  return t

someValue = logThenReturn "a string"

-- Tuple
t = ("key", 6)

-- List Comprehensions
numbers = [x | x <- [1..10]]
numbersSquared = [x * x | x <- [1..10]]

-- Recursion
power x y = powerIter x y 1
  
powerIter x y total
  | y <= 0 = 1 * total
  | otherwise = (powerIter x (y - 1) (total * x))