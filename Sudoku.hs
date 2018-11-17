--

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
              deriving Show

mkDigit :: Int -> Maybe Int
mkDigit x
  | x `elem` [1..9] = Just x
  | otherwise       = Nothing

checkSquare :: Maybe Int -> Bool
checkSquare Nothing = True
checkSquare (Just x) = checkDigitValid x

checkDigitValid :: Int -> Bool
checkDigitValid x = x `elem` [1..9]





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

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (take 9 (repeat (take 9 (repeat Nothing))))

isSudoku :: Sudoku -> Bool
isSudoku s = (length (x:xs) == 9) && rowLength (x:xs)
        where (x:xs) = rows s

rowLength :: [[Maybe Int]] -> Bool
rowLength [s] = and [ length x == 9 | x <- [s] ]

row :: Sudoku -> [Maybe Int]
row s = x
  where (x:xs) = rows s

-- | GETTERS

getRow :: Sudoku -> Int -> [Maybe Int]
getRow s x = r !! x
  where r = rows s

getColumn :: Sudoku -> Int -> [Maybe Int]
getColumn s x = getColumn' s x 0

getColumn' :: Sudoku -> Int -> Int -> [Maybe Int]
getColumn' s x index
 | index > 8 = []
 | otherwise = ((getRow s index) !! x) : getColumn' s x (index + 1)
 where r = rows s

getValue :: Sudoku -> Int -> Int -> Maybe Int
getValue s c r = (getRow s r) !! c

-- | PRINTING FUNCTIONS

rowToString :: [Maybe Int] -> String
rowToString []           = ""
rowToString (Nothing:rs) = " N " ++ (rowToString rs)
rowToString (Just x:rs)  = " " ++ (show x) ++ " " ++ (rowToString rs)

columnToString :: [Maybe Int] -> String
columnToString [] = ""
columnToString (Nothing:rs) = "N \n" ++ columnToString rs
columnToString (Just x:rs) = (show x) ++ "\n" ++ columnToString rs

printSudoku :: Sudoku -> IO()
printSudoku s = printSudoku' s 0

printSudoku' :: Sudoku -> Int -> IO()
printSudoku' s index
  | index > 8 = return ()
  | otherwise = do putStrLn (rowToString (getRow s index))
                   printSudoku' s (index+1)

printValue :: Maybe Int -> IO()
printValue Nothing = putStrLn "N"
printValue (Just x) = putStrLn (show x)
