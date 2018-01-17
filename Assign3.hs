data Sudoku = Sudoku { rows :: [[Maybe Int]]} 
              deriving Show

--Part A1
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku createL 

createL :: [[Maybe Int]]
createL = [m:xs |m <- take 9 (repeat Nothing)]
          where xs = [x| x<- take 9 (repeat Nothing)]

--Part A2          
isvalid :: Maybe Int -> Bool
isvalid Nothing        = True
isvalid (Just x)       = x `elem` [1,2,3,4,5,6,7,8,9]

checkRow :: [Maybe Int] -> Bool
checkRow []     = True
checkRow (x:xs) = isvalid x && checkRow xs

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku (Sudoku y)  = length(concat y) == 81 && checkRow (concat y) 


{-
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
-}

isFilled :: Sudoku -> Bool
isFilled (Sudoku []) = False
isFilled (Sudoku y)  = and [ isNothing (concat y)]    

isNothing :: [Maybe Int] -> Bool
isNothing [] = True
isNothing (y:ys) | y == Nothing = False
                 | otherwise    = isNothing ys

{-
example2 :: Sudoku
example2 =
    Sudoku
      [ [j 3,j 6,j 2  ,j 4  ,j 7,j 1,j 2,j 5  ,j 9  ]
      , [j 2  ,j 5,j 4  ,j 6  ,j 7  ,j 8  ,j 1,j 8,j 9  ]
      , [j 1  ,j 2  ,j 9,j 2,j 5  ,j 4,j 7,j 6  ,j 9  ]
      , [j 1  ,j 5  ,j 4  ,j 2  ,j 1,j 3,j 7  ,j 2,j 8]
      , [j 4,j 2  ,j 4  ,j 5,j 6  ,j 2,j 7  ,j 8  ,j 9]
      , [j 2,j 7,j 3  ,j 4,j 6,j 3  ,j 3  ,j 8  ,j 9  ]
      , [j 3  ,j 3  ,j 5,j 3,j 3  ,j 8,j 9,j 3  ,j 3  ]
      , [j 3  ,j 8,j 3,j 3  ,j 3  ,j 3  ,j 3  ,j 6,j 3  ]
      , [j 3  ,j 3  ,j 7,j 6,j 9,j 3  ,j 3  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just
-}             


