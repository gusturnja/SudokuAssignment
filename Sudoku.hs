--
import Data.List
import Data.Char
import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
              deriving Show

type Block = [Maybe Int]

mkDigit :: Int -> Maybe Int
mkDigit x
  | x `elem` [1..9] = Just x  --Can either be a number from 1 to 9 ...
  | otherwise       = Nothing -- ... or nothing. Any value outside of 1 to 9 defaults to nothing

example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

row1 :: [Maybe Int]
row1 = [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]

-- PART A1 ---------------------------------------------------------------------

-- | Creates an entirely blank sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- PART A2 ---------------------------------------------------------------------

-- | Checks to see if a cell contains either nothing or a valid integer
checkCell :: Maybe Int -> Bool
checkCell Nothing = True
checkCell (Just x) = x `elem` [1..9]

-- | Checks if all rows in the sudoku are valid (contain 9 cells and each cell is either a valid integer or nothing)
checkAllRowsValid :: Sudoku -> Bool
checkAllRowsValid s = and [ checkRowValid x | x <- r ]
  where r = rows s

-- | Checks a single row if it is valid (contain 9 cells and each cell is either a valid integer or nothing with no repeating integers)
checkRowValid :: [Maybe Int] -> Bool
checkRowValid r = length r == 9 && and [ checkCell x | x <- r ]

-- | Checks whether a given sudoku is valid (has 9 rows and 9 columns and each cell is either Nothing or an Integer from 1 to 9)
isSudoku :: Sudoku -> Bool
isSudoku s = checkAllRowsValid s && length r == 9
  where r = rows s

-- PART A3 ---------------------------------------------------------------------

-- | Checks if theres blanks within a given row
noBlanksInRow :: [Maybe Int] -> Bool
noBlanksInRow r = [ x  | x <- r,  x == Nothing] == []

-- | Checks all rows for blank cells
isFilled :: Sudoku -> Bool
isFilled s = and [ noBlanksInRow x | x <- r ]
  where r = rows s

-- PART B1 ---------------------------------------------------------------------

-- | Returns a row at a specified index
getRow :: Sudoku -> Int -> [Maybe Int]
getRow s x = r !! x
  where r = rows s

-- | Converts a row to a printable string
rowToString :: [Maybe Int] -> String
rowToString []           = ""
rowToString (Nothing:rs) = "." ++ (rowToString rs)
rowToString (Just x:rs)  = (show x) ++ (rowToString rs)

-- | Converts a row to a printable string
rowToStringAlt :: [Maybe Int] -> String
rowToStringAlt []           = ""
rowToStringAlt (Nothing:rs) = " . " ++ (rowToStringAlt rs)
rowToStringAlt (Just x:rs)  = " " ++ (show x) ++ " " ++ (rowToStringAlt rs)

-- | Print entire Sudoku
printSudoku :: Sudoku -> IO()
printSudoku s = printSudoku' s 0

-- | Helper function to printSudoku. Prints one row at a time starting at a specified row index.
printSudoku' :: Sudoku -> Int -> IO()
printSudoku' s index
  | index > 8 = return ()
  | otherwise = do putStrLn (rowToString (getRow s index))
                   printSudoku' s (index+1)

-- | Print entire Sudoku using the alternate row to string function as I find that easier to read
printSudokuAlt :: Sudoku -> IO()
printSudokuAlt s = printSudokuAlt' s 0

-- | Helper function to printSudoku. Prints one row at a time starting at a specified row index.
printSudokuAlt' :: Sudoku -> Int -> IO()
printSudokuAlt' s index
 | index > 8 = return ()
 | otherwise = do putStrLn (rowToStringAlt (getRow s index))
                  printSudokuAlt' s (index+1)

-- PART B2 ---------------------------------------------------------------------

parseToRow :: [Char] -> [Maybe Int]
parseToRow [] = []
parseToRow (x:xs)
  | x /= '.'  = Just (digitToInt x) : parseToRow xs
  | otherwise = Nothing : parseToRow xs

parseToSudoku :: [String] -> [[Maybe Int]]
parseToSudoku [] = []
parseToSudoku (x:xs) = parseToRow x : parseToSudoku xs

readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
  contents <- readFile fp
  let r = lines contents
  return (Sudoku (parseToSudoku r))

-- PART C1 ---------------------------------------------------------------------

cell :: Gen (Maybe Int)
cell = frequency [(1,rNumeric),(9,return Nothing)]

rNumeric :: Gen (Maybe Int)
rNumeric = elements [Just n|n<-[1..9]]

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

-- PART D1 ---------------------------------------------------------------------

isOkayBlock :: Block -> Bool
isOkayBlock b = length (cells) == length (nub (cells))
  where cells = filter (/= Nothing) b --remove all the Nothing values as they are allowed to be repeated

blocks :: Sudoku -> [Block]
blocks s = r ++ (transpose r)
  where r = rows s

doSomething :: Sudoku -> [[Maybe Int]]
doSomething s = joinBlocks a : joinBlocks b : joinBlocks c --a, b and c are each three rows
  where (a,b,c) = seperateIntoThreeRows s

joinBlocks :: [[Maybe Int]] -> [[Maybe Int]]
joinBlocks r = (a:a1:a2) : (b:b1:b2) : (c:c1:cs)
  where (a,b,c) = seperateRowIntoThree (r !! 0)
        (a1,b1,c1) = seperateRowIntoThree (r !! 1)
        (a2,b2,c2) = seperateRowIntoThree (r !! 2)

seperateIntoThreeRows :: Sudoku -> ([[Maybe Int]],[[Maybe Int]],[[Maybe Int]])
seperateIntoThreeRows s = (a,b,c)
  where (a,x) = splitAt 3 (rows s)
        (b,y) = splitAt 3 x
        (c,z) = splitAt 3 y

seperateRowIntoThree :: [Maybe Int] -> ([Maybe Int], [Maybe Int], [Maybe Int])
seperateRowIntoThree r = (a,b,c)
  where (a,x) = splitAt 3 r
        (b,y) = splitAt 3 x
        (c,z) = splitAt 3 y

-- | GETTERS

-- | Returns a colum at a specified index
getColumn :: Sudoku -> Int -> [Maybe Int]
getColumn s x = getColumn' s x 0

-- | Helper function for getColumn
getColumn' :: Sudoku -> Int -> Int -> [Maybe Int]
getColumn' s x index
 | index > 8 = []
 | otherwise = ((getRow s index) !! x) : getColumn' s x (index + 1)
 where r = rows s

-- | Returns the value within a specified cell
getValue :: Sudoku -> Int -> Int -> Maybe Int
getValue s c r = (getRow s r) !! c



-- | PRINTING FUNCTIONS



columnToString :: [Maybe Int] -> String
columnToString [] = ""
columnToString (Nothing:rs) = "N \n" ++ columnToString rs
columnToString (Just x:rs) = (show x) ++ "\n" ++ columnToString rs



printValue :: Maybe Int -> IO()
printValue Nothing = putStrLn "N"
printValue (Just x) = putStrLn (show x)

-- | OLD FUNCTIONS
-- | Helper function for rowFilled
rowFilled' :: [Maybe Int] -> [Int] -> Bool
rowFilled' [] found
  | sum found == sum [1..9] = True --If the sum of the found list is the same as the sum of the expected list, they must be the same. This presumes that the row being given is valid
  | otherwise = False
rowFilled' (Nothing:xs) found = False
rowFilled' ((Just x):xs) found
  | x `elem` found = False
  | otherwise      = rowFilled' xs (x:found)
