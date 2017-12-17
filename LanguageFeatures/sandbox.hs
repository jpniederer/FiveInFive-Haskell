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