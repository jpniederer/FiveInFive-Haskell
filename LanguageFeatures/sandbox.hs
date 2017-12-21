-- :l sandbox from within ghci to load the functions
doubleIt x = x * 2
squareIt x = x * x
sumOfSquares x y = squareIt x + squareIt y

trimBigNumber x = if x > 100
                    then x - 100
                    else x

combineLists a b = a ++ b

addToFront a b = a:b

-- List Functions
getFirstElement l = head l
getRest l = tail l
getLast l = last l
getAllButLast l = init l
getLength l = length l
isNull l = null l
reverseCollection l = reverse l
takeFirstX x l = take x l
dropFirstX x l = drop x l
getMax l = maximum l
getMin l = minimum l
getSum l = sum l
getProduct l = product l
aToZ = ['a'..'z']
oneToX x = [1..x]

-- List Comprehensions
doubleEvensToX x = [x * 2 | x <- [2,4..x]]

-- Tuples
getFirst t = fst t
getSecond t = snd t
buildTuples l1 l2 = zip l1 l2

power x y =
  if y == 0
  then 1
  else x * (power x (y - 1))

power2 x y = powerLoop x y 1
powerLoop x y val =
  if y > 0
  then powerLoop x (y - 1) (val * x)
  else val

-- Taken from Pluralsight course Haskell Fundamentals Part 1
removeOdd nums =
  if null nums
  then []
  else
    if (mod (head nums) 2) == 0
    then (head nums) : (removeOdd (tail nums))
    else removeOdd (tail nums)

-- Pattern Matching
removeOddPm [] = []
removeOddPm (x : xs) = 
  if (mod x 2) == 0 
  then x : (removeOddPm xs)
  else removeOddPm xs

-- Guards
pow2 n
  | n == 0 = 1
  | otherwise = 2 * (pow2 (n - 1))

removeOddGuard [] = []
removeOddGuard (x : xs)
  | mod x 2 == 0 = x : (removeOddGuard xs)
  | otherwise = removeOddGuard xs

allOdd nums = case (removeOddGuard nums) of
  [] -> True
  (x : xs) -> False

getStringLengths l = map length l

doubleElements = map (2*)

isEven x = mod x 2 == 0
removeOddFilter = filter isEven

doubleEvens l = map (2*) (filter isEven l)

foldSum l = foldl (+) 0 l

doubleLambda l = map (\ x -> 2 * x) l

--notNullComposition = not . null

-- Type Synonyms
type String' = [Char]
type Point = (Double, Double)

-- Newtype, extends an existing type
newtype CustomerNumber = MakeCustomerNumber [Char]
getCustomerNumber (MakeCustomerNumber n) = n

testCustomerNumber = 
  let c = MakeCustomerNumber "abc123"
  in getCustomerNumber c

-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSet = quicksort [a | a <- xs, a <= x]
      biggerSet = quicksort [a | a <- xs, a > x]
  in smallerSet ++ [x] ++ biggerSet

-- Records
data Customer = MakeCustomer
  {
    customerNumber :: CustomerNumber
  , name :: String
  , luckyNumber :: Int
  }

josh :: Customer
josh = MakeCustomer
  { customerNumber = MakeCustomerNumber "Josh123"
  , name = "Josh"
  , luckyNumber = 255
  }

-- name josh -> "Josh"
-- luckyNumber josh -> 255
-- use algebraic data types over records.

-- Algebraic Data Types
data CustomerAc = CustomerAc CustomerNumber String Int
joshAc :: CustomerAc
joshAc = CustomerAc (MakeCustomerNumber "Josh123") "Josh" 255
getCustomerNumberAc :: CustomerAc -> CustomerNumber
getCustomerNumberAc (CustomerAc cust_number _ _) = cust_number
