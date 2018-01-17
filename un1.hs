import Data.Char
import Data.List
import Test.QuickCheck
------------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]]} 
              deriving ( Show, Eq )


-- | A sample sudoku puzzle
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

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku createL 

createL :: [[Maybe Int]]
createL = [m:xs |m <- take 9 (repeat Nothing)]
          where xs = [x| x<- take 9 (repeat Nothing)]

-- * A2          
isvalid :: Maybe Int -> Bool
isvalid Nothing        = True
isvalid (Just x)       = x `elem` [1..9]

checkRow :: [Maybe Int] -> Bool
checkRow []     = True
checkRow (x:xs) = isvalid x && checkRow xs

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku (Sudoku y)  = length(concat y) == 81 && checkRow (concat y) 


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku []) = False
isFilled (Sudoku y)  = and [ isNothing (concat y)]    

isNothing :: [Maybe Int] -> Bool
isNothing [] = True
isNothing (y:ys) | y == Nothing = False
                 | otherwise    = isNothing ys
       

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO()
printSudoku (Sudoku y) = putStrLn(printMat y)

-- | maps the print helper function onto all elements of Sudoku
printMat :: [[Maybe Int]] -> [Char]
printMat (y:[]) = map printelem y
printMat (y:ys) = (map printelem y ++ "\n" ++ printMat ys)

-- | prints corresponding element
printelem :: Maybe Int -> Char
printelem Nothing = '.'
printelem (Just n)  = intstr n

-- | matches the number with character
intstr :: Int -> Char
intstr n = head(drop (n-1) ['1'..'9'])

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku

readSudoku :: FilePath -> IO Sudoku
readSudoku y = do ys <- readFile y
                  let ys1 = lines ys
                  if (length ys1 /= 9)
                    then error "Invalid File"
                    else return (Sudoku (createSud ys1))

-- | Creates the List for Sudoku
createSud :: [String] -> [[Maybe Int]]
createSud (y:[]) = [concat(retnum y)]
createSud (y:ys) = concat(retnum y):createSud ys

-- | Creates Sudoku elements from Strings
retnum :: [Char] -> [[Maybe Int]]
retnum (x:[]) | x == '.'            = [[Nothing]]
              | x `elem` ['1'..'9'] = [[Just (digitToInt x)]]
retnum (x:xs) | x == '.'            = [Nothing]:retnum xs
              | x `elem` ['1'..'9'] = [Just (digitToInt x)]:retnum xs
                

-- * C1

-- | rand generate random numbers for cells in a Sudoku
randn :: Gen (Maybe Int)
randn = elements[Just n| n<-[1..9]]

-- | cell generates an arbitrary cell in a Sudoku
cell:: Gen (Maybe Int)
cell = frequency[(2,randn),(8,return Nothing)]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
    arbitrary =
      do rows <- vectorOf 9 (vectorOf 9 cell)
         return (Sudoku rows)
  
-- * C3

-- | checks that generated Sudoku is a Sudoku according to A2
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s


-- * D1
type Block = [Maybe Int]

-- | Checks if same digit is not repeated
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:xs) | elem x xs && x /= Nothing = False
                   | otherwise                 = isOkayBlock xs


-- * D2

-- | Creates a list of all row, column and 3x3 blocks of a Sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku ys) = ys ++ transpose (ys) ++ cellsud (ys)

-- | Appends the 3x3 blocks created by supportcell
cellsud :: [[Maybe Int]] -> [Block] 
cellsud (y1:y2:y3:[]) = supportcell y1 y2 y3
cellsud (y1:y2:y3:ys) = supportcell y1 y2 y3 ++ cellsud ys

-- | Creates 3 blocks of 3x3 cells from 3 rows of Sudoku
supportcell :: [Maybe Int] -> [Maybe Int] -> [Maybe Int] -> [[Maybe Int]]
supportcell [] [] [] = []
supportcell v1 v2 v3 = [take 3 v1 ++ take 3 v2 ++ take 3 v3] ++ supportcell (drop 3 v1) (drop 3 v2) (drop 3 v3)

-- | Counds the number of elements in Sudoku
prop_blocks' :: [Block] -> Int
prop_blocks' [] = 0
prop_blocks' (x:xs) = length x + prop_blocks' xs

-- | Checks that a Sudoku has 3x9 blocks and each block as 9 cells
prop_blocks :: Sudoku -> Bool
prop_blocks s = length b==27 && (prop_blocks' b == 243)
  where b = blocks s

-- * D3

-- | Checks that all rows, columns and 3x3 blocks don't have repeated digits
isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock b | b <- (blocks s)]