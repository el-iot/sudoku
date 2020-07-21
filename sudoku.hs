puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]

serialisedPuzzle = "530070000600195000098000060800060003400803001700020006060000280000419005000080079"

type Sudoku = [[Integer]]
type Coordinates = (Int, Int)
type SerialisedSudoku = String

deserialise :: SerialisedSudoku -> Sudoku
deserialise s
    |  [] <- s   = []
    |  otherwise = [[read [e] :: Integer | e <- take 9 s]] ++ deserialise (drop 9 s)

serialise :: Sudoku -> SerialisedSudoku
serialise x = helper $ flattenValues x
    where
  helper x
    |  [] <- x   = []
    |  otherwise = show (x !! 0) ++ helper (tail x)

flattenValues :: [[Integer]] -> [Integer]
flattenValues x
  |  [] <- x    = []
  |  otherwise  = (x !! 0) ++ flattenValues (tail x)

(?) :: (Eq x) => x -> [x] -> Bool
(?) element array
  |  [] <- array = False
  |  otherwise   = (array !! 0 == element) || element ? (tail array)

missingFrom :: (Eq x, Num x, Enum x) => [x] -> [x]
missingFrom x = filter (\e -> not (e ? x)) [1..9]

getRow :: Int -> Sudoku -> [Integer]
getRow idx sudoku = missingFrom (sudoku !! idx)

getColumn :: Int -> Sudoku -> [Integer]
getColumn idx sudoku = missingFrom [row !! idx | row <- sudoku]

getSquare :: Coordinates -> Sudoku -> [Integer]
getSquare (x, y) sudoku = missingFrom $ [sudoku !! yy !! xx | yy <- [y..(y+2)], xx <- [x..(x+2)]]

getIntersection :: (Eq t) => [t] -> [t] -> [t] -> [t]
getIntersection x y z = helper x (helper y z)
    where
  helper x y = filter (\e -> e ? x) y

updateAt :: Coordinates -> Integer -> Sudoku -> Sudoku
updateAt (x, y) value sudoku = replaceValue y  (replaceValue x value (sudoku !! y)) sudoku
    where
  replaceValue :: Int -> x -> [x] -> [x]
  replaceValue idx element array = helper 0 idx element array
      where
    helper current target element array
      |  [] <- array       = []
      |  current == target = [element] ++ helper (current + 1) target element (tail array)
      |  otherwise         = [array !! 0] ++ helper (current + 1) target element (tail array)

solveAtCoords :: Coordinates -> Sudoku -> Sudoku
solveAtCoords (x, y) sudoku
  |  (sudoku !! y !! x == 0) && (length options == 1) = updateAt (x, y) (options !! 0) sudoku
  |  otherwise                                        = sudoku
    where
  options = getIntersection (getRow y sudoku) (getColumn x sudoku) (getSquare (xx, yy) sudoku)
  xx      = 3*(div x 3)
  yy      = 3*(div y 3)

solve :: Sudoku -> Sudoku
solve sudoku = walk (0, 0) sudoku
    where
  walk :: Coordinates -> Sudoku -> Sudoku
  walk (x, y) sudoku
    |  isComplete sudoku           = sudoku
    |  otherwise                   = walk (updateCoords (x, y)) (solveAtCoords (x, y) sudoku)
  isComplete :: Sudoku -> Bool
  isComplete sudoku = not $ 0 ? flattenValues sudoku
  updateCoords :: Coordinates -> Coordinates
  updateCoords (x, y)
    |  (x == 8) && (y == 8) = (0, 0)
    |  (x == 8)             = (0, y + 1)
    |  otherwise            = (x + 1, y)

solveSerialised:: SerialisedSudoku -> SerialisedSudoku
solveSerialised s = serialise $ solve $ deserialise s
