module Sudoku where
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
            deriving Show

type Block = [Maybe Int]

type Pos = (Int,Int)
--column number then row number. Think along the corridor and up the stairs.

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

-- | USED FOR TESTING
row1 :: Block
row1 = [Just 1, Nothing, Just 3, Just 4, Just 5, Nothing, Just 7, Just 8, Just 9]

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
checkAllRowsValid s = all checkRowValid (rows s)

-- | Checks a single row if it is valid (contain 9 cells and each cell is either a valid integer or nothing with no repeating integers)
checkRowValid :: [Maybe Int] -> Bool
checkRowValid r = length r == 9 && all checkCell r

-- | Checks whether a given sudoku is valid (has 9 rows and 9 columns and each cell is either Nothing or an Integer from 1 to 9)
isSudoku :: Sudoku -> Bool
isSudoku s = checkAllRowsValid s && length (rows s) == 9

-- PART A3 ---------------------------------------------------------------------

-- | Checks if theres blanks within a given row
noBlanksInRow :: [Maybe Int] -> Bool
noBlanksInRow r = null [ x  | x <- r, isNothing x]

-- | Checks all rows for blank cells
isFilled :: Sudoku -> Bool
isFilled s = all noBlanksInRow r
  where r = rows s

-- PART B1 ---------------------------------------------------------------------

-- | Converts a row to a printable string
rowToString :: [Maybe Int] -> String
rowToString []           = ""
rowToString (Nothing:rs) = "." ++ rowToString rs
rowToString (Just x:rs)  = show x ++ rowToString rs

-- | Print entire Sudoku
printSudoku :: Sudoku -> IO()
printSudoku s = putStrLn (unlines (map rowToString (rows s)))

-- PART B2 ---------------------------------------------------------------------

-- | Transfer a given string into a valid sudoku row
parseToRow :: String -> [Maybe Int]
parseToRow [] = []
parseToRow (x:xs)
  | x /= '.'  = Just (digitToInt x) : parseToRow xs
  | otherwise = Nothing : parseToRow xs

-- | Read a sudoku from a given filepath
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
  contents <- readFile fp
  let r = lines contents
  return (Sudoku (map parseToRow r))

-- PART C1 ---------------------------------------------------------------------

cell :: Gen (Maybe Int)
cell = frequency [(1,rNumeric),(9,return Nothing)]

rNumeric :: Gen (Maybe Int)
rNumeric = elements [Just n|n<-[1..9]]

-- PART C2 ---------------------------------------------------------------------

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- PART C3 ---------------------------------------------------------------------

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-- PART D1 ---------------------------------------------------------------------

-- | Returns true if there are no repeating digits in a block (repeated Nothings are ok) or False if there are repeated values
isOkayBlock :: Block -> Bool
isOkayBlock b = length cells == length (nub cells)
  where cells = filter (/= Nothing) b --remove all the Nothing values as they are allowed to be repeated

-- PART D2 ---------------------------------------------------------------------

-- | Generate a list of all of the rows, columns and blocks in a sudoku
blocks :: Sudoku -> [Block]
blocks s = r ++ transpose r ++ getBlocks s
  where r = rows s

-- | Split a sudoku into its 9 3*3 blocks
getBlocks :: Sudoku -> [[Maybe Int]]
getBlocks s = join a ++ join b ++ join c --a, b and c are each three rows
  where (a,b,c) = splitInto3 (rows s)
        join :: [[Maybe Int]] -> [[Maybe Int]]
        join r = [a++a1++a2,b++b1++b2,c++c1++c2]
          where (a,b,c)    = splitInto3 (head r)
                (a1,b1,c1) = splitInto3 (r !! 1)
                (a2,b2,c2) = splitInto3 (r !! 2)

-- | Splits a list into 3 even lists
splitInto3 :: [a] -> ([a],[a],[a])
splitInto3 r = (l,m,n)
  where (l,x) = splitAt 3 r
        (m,y) = splitAt 3 x
        (n,z) = splitAt 3 y


prop_block_lengths :: Sudoku -> Bool
prop_block_lengths s = length b == 3*9 && all prop_row_length b
  where b = blocks s

prop_row_length :: Block -> Bool
prop_row_length b = length b == 9

-- PART D3 ---------------------------------------------------------------------

-- | Check all blocks in a sudoku are valid (contain no repeating values)
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- PART E ----------------------------------------------------------------------

-- | Returns a row at a specified index
getRow :: Sudoku -> Int -> [Maybe Int]
getRow s x = r !! x
  where r = rows s

-- | Returns the value within a specified cell
getValue :: Sudoku -> Pos -> Maybe Int
getValue s (c,r) = getRow s r !! c

-- | Identifies the location of all the blanks in the sudoku puzzle
blanks :: Sudoku -> [Pos]
blanks s = zip (checkAllForNothing r) (sort (checkAllForNothing c))
  where r = rows s
        c = transpose r

-- | Check a given list of rows/columns for the location of the blanks, returning a list of all the indexes found
checkAllForNothing :: [[Maybe Int]] -> [Int]
checkAllForNothing [] = []
checkAllForNothing (x:xs) = foldr ((++) . checkRowForNothing) [] xs

-- | Check an individual row/column for blanks and return a list of their indexes
checkRowForNothing :: [Maybe Int] -> [Int]
checkRowForNothing [] = []
checkRowForNothing (x:xs)
  | isNothing x = (8 - length xs) : checkRowForNothing xs
  | otherwise    = checkRowForNothing xs

-- | Update a given list by replacing the item at the specified location with the new specified item
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_,x) = [x]
(!!=) xs (i,x) = (take i xs) ++ [x] ++ (drop (i+1) xs)

-- | Update an location within a sudoku with a new specified item
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (x,y) n = Sudoku (r !!= (y , (r !! y) !!= (x,n)))
  where r = rows s

-- TODO incorporate the 3x3 block into this
candidate :: Sudoku -> Pos -> [Int]
candidate s (x,y) = intersect (intersect (findCandidates (r !! y)) (findCandidates ((transpose r) !! x))) (findCandidates (findBlock s (x,y)))
  where r = rows s

-- | Finds all possible value in a given block
findCandidates :: Block -> [Int]
findCandidates r = [ x | x <- [1..9], not (x `elem` xs) ]
  where xs = catMaybes r

findBlock :: Sudoku -> Pos -> Block
findBlock s (x,y) = head (getThird (getThird (getBlocks s) y) x) --get the head as there should only be one item in the list
  where getThird :: [a] -> Int -> [a]
        getThird xs x
          | x < 3 = take lb xs --take first third
          | x > 5 = drop ub xs --take last third
          | otherwise = drop lb (take ub xs) --take mid third
          where lb = (length xs `div` 3)
                ub = lb * 2

-- PART F ----------------------------------------------------------------------
