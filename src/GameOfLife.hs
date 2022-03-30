module GameOfLife where

import Graphics.Gloss.Interface.Pure.Game
import Text.Read
import Indexing

-- Cell state
data State = Alive | Dead deriving Eq

-- Board - array of cells of set size
data Board = Board {
  size :: Int,
  cells :: [State]}
  --possibly current iteration

-- Errors while parsing
data Error = ConfigurationError String | SizeError String

-- Display mode
display :: Display
display = FullScreen

-- Background colour
bgColor :: Color
bgColor = azure

-- Update frequency
fps :: Int
fps = 1

-- Screen size in pixels
screenSize :: Int
screenSize = 1000

-- Parse text and get board configuration
setBoard :: String -> Either Error Board
setBoard str =
  let (size_str : cells) = lines str
  in case readMaybe size_str :: Maybe Int of
    Nothing -> Left $ ConfigurationError "Size required on line 1"
    Just size -> do
      if size < 3 
        then Left $ SizeError "Size must be >= 3"
        else if (length cells) /= size^2
          then Left $ ConfigurationError "First line should be followed up with size^2 lines"
          else if not $ all (\x -> x) (map (\x -> ((x == "0") || (x == "1"))) cells)
            then Left $ ConfigurationError "Each line after #1 should just contain 0 or 1"
            else Right $ Board size (map (\x -> if x == "1" then Alive else Dead) cells)

-- Count alive neighbours of cell
countAliveNeighbours :: Board -> [Int] -> Int
countAliveNeighbours (Board _ cells) neighbours = 
  foldr (\x r -> if x == Alive then r + 1 else r) 0 (map (cells !!) neighbours)

-- Update cell state by its current state and neighbours' states
updateCell :: State -> Int -> State
updateCell Dead 3 = Alive
updateCell Alive 2 = Alive
updateCell Alive 3 = Alive
updateCell _ _ = Dead

-- Check if all cells died :c
checkEndGame :: Board -> Bool
checkEndGame (Board size cells) = (countAliveNeighbours (Board size cells) [0..(size^2 - 1)]) == 0

-- Update all cells on board
updateBoard :: Float -> Board -> Board
updateBoard _ (Board size cells) = 
  Board size ( map 
                (\i -> updateCell (cells !! i) 
                                 (countAliveNeighbours (Board size cells) (neighbours size i)))
                [0..(size^2 - 1)]
             )

-- Get cell state by its position in array
getState :: Int -> Board -> State
getState pos (Board size (x:xs)) | pos > 0 = getState (pos - 1) (Board size xs)
                               | otherwise = x

-- Draw a picture of current board state
drawApp :: Board -> Picture
drawApp (Board size cells) = Pictures [
  Color (if (getState j (Board size cells)) == Dead then black else white) $ Polygon $ toFloat
    [(upperLeftX + (j `mod` size) * squareSize, upperLeftY - (j `div` size) * squareSize),
    (upperLeftX + (j `mod` size + 1) * squareSize, upperLeftY - (j `div` size) * squareSize),
    (upperLeftX + (j `mod` size + 1) * squareSize, upperLeftY - (j `div` size + 1) * squareSize),
    (upperLeftX + (j `mod` size) * squareSize, upperLeftY - (j `div` size + 1) * squareSize)]
   | j <- [0..(size^2 - 1)]]
  where
    squareSize = screenSize `div` size
    upperLeftX = - (screenSize `div` 2)
    upperLeftY = screenSize `div` 2

toFloat :: [(Int, Int)] -> [(Float, Float)]
toFloat [] = []
toFloat ((a, b) : xs) = (fromIntegral a :: Float, fromIntegral b :: Float) : (toFloat xs)

-- Handle IO events
handleEvent :: Event -> Board -> Board
handleEvent _ board = board

run :: IO ()
run = do
  boardConfFile <- getLine
  configuration <- readFile boardConfFile
  case setBoard configuration of
    Left (ConfigurationError message) -> putStrLn message
    Left (SizeError message) -> putStrLn message
    Right board -> do
      play display bgColor fps board drawApp handleEvent updateBoard
