module Sudoku where
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
            deriving (Show,Eq)

type Block = [Maybe Int]

type Pos = (Int,Int)
--row number, then column number.

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

-- PART A1 ---------------------------------------------------------------------

-- | Creates an entirely blank sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- PART A2 ---------------------------------------------------------------------

-- | Checks to see if a cell contains either nothing or a valid integer
checkCell :: Maybe Int -> Bool
checkCell Nothing  = True
checkCell (Just x) = x <= 9 && x >= 1

-- | Checks if all rows in the sudoku are valid (contain 9 cells and each cell is either a valid integer or nothing)
checkAllRowsValid :: Sudoku -> Bool
checkAllRowsValid sud = all checkRowValid (rows sud)

-- | Checks a single block if it is valid (contain 9 cells and each cell is either a valid integer or nothing with no repeating integers)
checkRowValid :: Block -> Bool
checkRowValid block = length block == 9 && all checkCell block

-- | Checks whether a given sudoku is valid (has 9 rows and 9 columns and each cell is either Nothing or an Integer from 1 to 9)
isSudoku :: Sudoku -> Bool
isSudoku sud = checkAllRowsValid sud && length (rows sud) == 9

-- PART A3 ---------------------------------------------------------------------

-- | Returns true if there are no blanks in a given row, otherwise false
noBlanksInRow :: Block -> Bool
noBlanksInRow row = all isJust row

-- | Checks all rows for blank cells
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = all noBlanksInRow rows

-- PART B1 ---------------------------------------------------------------------

-- | Converts a row to a printable string
rowToString :: Block -> String
rowToString []           = ""
rowToString (Nothing:rs) = "." ++ rowToString rs
rowToString (Just x:rs)  = show x ++ rowToString rs

-- | Print entire Sudoku
printSudoku :: Sudoku -> IO()
printSudoku sud = putStrLn (unlines (map rowToString (rows sud)))

-- PART B2 ---------------------------------------------------------------------

-- | Transfer a given string into a valid sudoku row
parseToRow :: String -> Block
parseToRow [] = []
parseToRow (value:rs)
  | value /= '.'  = Just (digitToInt value) : parseToRow rs
  | otherwise     = Nothing : parseToRow rs

-- | Read a sudoku from a given filepath
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
  contents <- readFile fp
  let rows = lines contents
  return (Sudoku (map parseToRow rows))

-- PART C1 ---------------------------------------------------------------------

cell :: Gen (Maybe Int)
cell = frequency [(1,rNumeric),(9,return Nothing)]

rNumeric :: Gen (Maybe Int)
rNumeric = elements [Just n| n<-[1..9]]

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
isOkayBlock block = length cells == length (nub cells)
  where cells = filter (isJust) block --remove all the Nothing values as they are allowed to be repeated

-- PART D2 ---------------------------------------------------------------------

-- | Generate a list of all of the rows, columns and blocks in a sudoku
blocks :: Sudoku -> [Block]
blocks sud = r ++ transpose r ++ getBlocks sud
  where r = rows sud

-- | Split a sudoku into its 9 3*3 blocks
getBlocks :: Sudoku -> [Block]
getBlocks sud = join a ++ join b ++ join c --a, b and c are each three rows
  where (a,b,c) = splitInto3 (rows sud)
        join :: [Block] -> [Block]
        join r = [a++a1++a2,b++b1++b2,c++c1++c2]
          where (a,b,c)    = splitInto3 (head r)
                (a1,b1,c1) = splitInto3 (r !! 1)
                (a2,b2,c2) = splitInto3 (r !! 2)

-- | Splits a list of length 9 into 3 even lists
splitInto3 :: [a] -> ([a],[a],[a])
splitInto3 r = (x1,x2,x3)
  where (x1,y)  = splitAt third r
        (x2,x3) = splitAt third y
        third   = (length r) `div` 3

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length b == 3*9 && all prop_row_length b
  where b = blocks s

prop_row_length :: Block -> Bool
prop_row_length b = length b == 9

-- PART D3 ---------------------------------------------------------------------

-- | Check all blocks in a sudoku are valid (contain no repeating values)
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- PART E1 ---------------------------------------------------------------------

-- | Returns a row at a specified index
getRow :: Sudoku -> Int -> [Maybe Int]
getRow s x = r !! x
  where r = rows s

-- | Returns the value within a specified cell
getValue :: Sudoku -> Pos -> Maybe Int
getValue s (c,r) = getRow s r !! c

--Function recieved as part of 3A feedback. I did not program this myself.
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) =  [ (rowNum, colNum) | (rowNum, row) <- zip [0..] rows, (colNum, cell) <- zip [0..] row, isNothing cell]

prop_blanks_allBlank :: Bool
prop_blanks_allBlank = length b == 81
  where b = blanks allBlankSudoku

-- PART E2 ---------------------------------------------------------------------

-- | Update a given list by replacing the item at the specified location with the new specified item
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_,x) = [x]
(!!=) xs (i,x) = (take i xs) ++ [x] ++ (drop (i+1) xs)

prop_bangBangEquals_correct :: NonEmptyList Int -> Int -> Bool
prop_bangbangEquals_correct (NonEmpty xs) value = length updatedxs == length xs && (updatedxs !! index) == value
  where updatedxs = xs !!= (index,value)
        index = (length xs) - 1

-- PART E3 ---------------------------------------------------------------------

-- | Update an location within a sudoku with a new specified item
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (y,x) n = Sudoku (r !!= (y , (r !! y) !!= (x,n)))
  where r = rows s

prop_update_updated :: Sudoku -> Maybe Int -> Pos -> Bool
prop_update_updated sud value p = isSudoku updatedsud && getValue updatedsud p == value
  where updatedsud = update sud p value

-- PART E4 ---------------------------------------------------------------------

-- TODO incorporate the 3x3 block into this
candidates :: Sudoku -> Pos -> [Int]
candidates s (y,x) = intersect (intersect (findCandidates (r !! y)) (findCandidates ((transpose r) !! x))) (findCandidates (findBlock s (y,x)))
  where r = rows s

-- | Finds all possible value in a given block
findCandidates :: Block -> [Int]
findCandidates r = [ x | x <- [1..9], not (x `elem` xs) ]
  where xs = catMaybes r

-- | Find the block that the given position is in
findBlock :: Sudoku -> Pos -> Block
findBlock s (y,x) = head (getThird (getThird (getBlocks s) y) x) --get the head as there should only be one item in the list
  where getThird :: [a] -> Int -> [a]
        getThird list index
          | index < 3 = a --take first third
          | index > 5 = c --take last third
          | otherwise = b --take mid third
          where (a,b,c) = splitInto3 list

prop_candidates_correct :: Sudoku -> Bool
prop_candidates_correct sud = and [  prop_candidates_correct' (candidates sud (y,x)) | (y,x) <- (blanks sud) ]

prop_candidates_correct' :: [Int] -> Bool
prop_candidates_correct' xs = l <= 9 && l == length (nub xs)
  where l = length xs

-- PART F1 ---------------------------------------------------------------------

-- | Attempt to solve a sudoku puzzle if the sudoku is valid. This returns Nothing if there is no solution or the sudoku is invalid
solve :: Sudoku -> Maybe Sudoku
solve s = if isSudoku s then do solve' (Just s) (blanks s) else do Nothing -- If the sudoku is valid, attempt to solve, otherwise return nothing

-- | Find candidates for a blank position in the sudoku and try to solve for that position
solve' :: Maybe Sudoku -> [Pos] -> Maybe Sudoku
solve' s [] = s --No blanks left, I have reached a potentially solved sudoku
solve' (Just s) (p:ps) = cycleCandidates (Just s) (p:ps) (candidates s p) -- I still have blanks, I will now try to update with a candidate

-- | Cycle through the possible candidates.
cycleCandidates :: Maybe Sudoku -> [Pos] -> [Int] -> Maybe Sudoku
cycleCandidates _ _ [] = Nothing -- I have run out of possible candidates for this position. This is a dead end, I must backtrack
cycleCandidates (Just s) (p:ps) (c:cs)
  | isNothing news && cs /= [] = cycleCandidates (Just s) (p:ps) cs -- I arrived at a dead end somewhere but I have more candidates to try before this is deemed unsolvable
  | isNothing news && cs == [] = Nothing -- I have reached a dead end and I have no other candidates to check so I backtrack further
  | otherwise                  = news -- I have reached a solved sudoku, the solved sudoku must be passed all the way back to the top of the stack
  where news = solve' (Just (update s p (Just c))) ps -- attempt to solve the sudoku with this candidate

-- PART F2 ---------------------------------------------------------------------

-- | Read a sudoku from a file, solve it if possible and print the solution
readAndSolve :: FilePath -> IO ()
readAndSolve f = do
                  sud <- readSudoku f
                  let solvedSud = solve sud
                  if isNothing solvedSud then do
                    putStrLn "(no solution)"
                  else do
                    printSudoku (fromJust solvedSud)

-- PART F3 ---------------------------------------------------------------------

-- | Check to see if one sudoku is a solution of another
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isOkay s1 && isFilled s1 && checkValues (getValuesLocations (rows s2)) s1

-- | Checks a list of values with their respective locations of one sudoku to see if they match the values at the same location in another sudoku
checkValues :: [(Pos,Maybe Int)] -> Sudoku -> Bool
checkValues [] _ = True
checkValues ((pos,value):xs) s
  | value == getValue s pos = checkValues xs s
  | otherwise = False

-- | Return a list of all values in a sudoku which arent nothing with their respective positions
getValuesLocations :: [[Maybe Int]] -> [(Pos,Maybe Int)]
getValuesLocations rows = [ ((rowNum,colNum),value) | (rowNum, row) <- (zip [0.. ] rows), (colNum,value) <- (zip [0..] row), isJust value ]

-- PART F4 ---------------------------------------------------------------------

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = not (isNothing solved) ==> isSolutionOf (fromJust solved) s
  where solved = solve s

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 3 } prop
