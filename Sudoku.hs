--
import Data.List
import Data.Char

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
              deriving Show

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

-- | Check a list of cells for any repeated integers, repeated nothings are allowed
noRepeatingValues :: [Maybe Int] -> Bool
noRepeatingValues r = noRepeatingValues' r []

-- | Helper function for noRepeatingValues
noRepeatingValues' :: [Maybe Int] -> [Int] -> Bool
noRepeatingValues' [] _ = True --No repeating values have been found
noRepeatingValues' (Nothing:xs) found = noRepeatingValues' xs found --ignore Nothings as they can be repeated
noRepeatingValues' ((Just x):xs) found
  | x `elem` found = False --If value has already been added to the list of found values, the value has been repeated
  | otherwise      = noRepeatingValues' xs (x:found)

-- | Checks to see if a valid row or column contains all possible digits with no repeats or nothings
rowFilled :: [Maybe Int] -> Bool
rowFilled r = noRepeatingValues r && checkRowValid r && noBlanksInRow r

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
readSodoku fp = do
  contents <- readFile fp
  let r = lines contents
  return (Sudoku (parseToSudoku r))

-- PART C1 ---------------------------------------------------------------------


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

-- | Converts a sudoku from a list of rows to a list of columns (can be used for checking the validity of all rows)
convertToColumns :: Sudoku -> Sudoku
convertToColumns s = Sudoku (convertToColumns' s 0)

convertToColumns' :: Sudoku -> Int -> [[Maybe Int]]
convertToColumns' s index
  | index > 8 = []
  | otherwise = (getColumn s index) : (convertToColumns' s (index + 1))

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
