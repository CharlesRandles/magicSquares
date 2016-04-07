import Data.List (transpose)

type Square = [[Int]]

tab :: String
lf :: String
tab = '\t':[]
lf = '\n':[]
showSquare :: Square -> String
showSquare rs = concat [showElems r ++ lf | r <- rs]
showElems :: (Show a) => [a] => String
showElems xs = concat [(show x) ++ tab | x <- xs]

--Constructors and aaccessors
makeSquare :: Int -> [Int] -> Square
size :: Square -> Int
rows :: Square -> [[Int]]
cols :: Square -> [[Int]]
diags:: Square -> [[Int]]
allLines :: Square -> [[Int]]

size (r:rs) = length r
rows rs = rs
cols = transpose . rows
diags rs = [[((rs!!)n!!)n | n <- [0..(size rs) - 1]] ,
           [((rs!!)n!!)(maxIndex - n) | n <- [0..maxIndex]]]
  where maxIndex = (size rs) - 1

allLines s = rows s ++ cols s ++ diags s

makeSquare _ [] =  []
makeSquare n xs =  (take n xs): (makeSquare n (drop n xs))

--Utility Function
allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x:[]) = True
allEqual (x:y:zs) = (x==y) && allEqual (y:zs)

--Is it a magic square?
isMagic :: Square -> Bool
isMagic s = allEqual $ [sum l | l <- allLines s]

--If we're ginve a prtial square with one
--row or col missing, what is it's constant?
--partialSquareSum :: Square -> Int
partialSquareSum s
  | s==[] = 0
  | (length s) > (length (s!!0)) = sum [head r | r <- s]
  | otherwise = sum $ head s

completeToSum :: Int -> [Int] -> [Int]
completeToSum magicSum row = row ++ (c:[])
                             where c = magicSum - (sum row)

completePartialRows :: Square -> Square
completePartialRows s = [completeToSum magicSum  row | row  <- s]
                        where magicSum = partialSquareSum s

completePartialCols :: Square -> Square
completePartialCols = transpose.completePartialRows.transpose

completePartial :: Square -> Square
completePartial s
  | s== [] = []
  | (length s) > (length (s!!0)) = completePartialRows s
  | (length s) < (length (s!!0)) = completePartialCols s
  | otherwise = s --Already square

canComplete :: Square -> Bool
canComplete = isMagic.completePartial

--Test data
ts1 = makeSquare 3 [8, 1, 6, 3, 5, 7, 4, 9, 2]
ts2 = makeSquare 3 [2, 7, 6, 9, 5, 1, 4, 3, 8]
ts3 = makeSquare 3 [3, 5, 7, 8, 1, 6, 4, 9, 2]
ts4 = makeSquare 3 [8, 1, 6, 7, 5, 3, 4, 9, 2]

ps1 = [[8,1], [3,5], [4,9]]::[[Int]]
ps2= [[3,5], [8,1], [4,9]]::[[Int]]
ps3=[[8,1,6], [3,5,7]]::[[Int]]
ps4=[[3,5,7], [8,1,6]]::[[Int]]
