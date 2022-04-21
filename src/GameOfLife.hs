module GameOfLife where

import Graphics.Gloss.Interface.Pure.Game
import Text.Read
import Indexing
import System.IO

import Types
import Consts


-- Parse text and get board configuration
setBoard :: String -> Either Error Board
setBoard [] = Left $ ConfigurationError emptyBoardMsg
setBoard str =
  let (size_str : cells) = lines str
  in case readMaybe size_str :: Maybe Int of
    Nothing -> Left $ ConfigurationError dimErrorMsg
    Just size -> do
      if size < 3 
        then Left $ SizeError dimRestrMsg
        else if (length cells) /= size * size
          then Left $ ConfigurationError lineCountMsg
          else if not $ all (\x -> x) (map (\x -> (x == "0") || (x == "1")) cells)
            then Left $ ConfigurationError lineStateMsg
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
checkEndGame board@(Board size _) = (countAliveNeighbours board [0..(size * size - 1)]) == 0

-- Update all cells on board
updateBoard :: Float -> Board -> Board
updateBoard _ board@(Board size cells) = 
  Board size (
               map 
               (\i -> updateCell (cells !! i) (countAliveNeighbours board (findNeighbours size i)))
               [0..(size * size - 1)]
             )

-- Get cell state by its position in array
getState :: Int -> Board -> State
getState pos (Board size (x:xs)) | pos > 0 = getState (pos - 1) (Board size xs)
                                 | otherwise = x
getState _ _ = Dead

-- Draw a picture of current board state
drawApp :: Board -> Picture
drawApp board@(Board size _) = Pictures [
  Color (if (getState j board) == Dead then deadColor else aliveColor) $ Polygon $ toFloat
    [(upperLeftX + (j `mod` size) * squareSize, upperLeftY - (j `div` size) * squareSize),
    (upperLeftX + (j `mod` size + 1) * squareSize, upperLeftY - (j `div` size) * squareSize),
    (upperLeftX + (j `mod` size + 1) * squareSize, upperLeftY - (j `div` size + 1) * squareSize),
    (upperLeftX + (j `mod` size) * squareSize, upperLeftY - (j `div` size + 1) * squareSize)]
    | j <- [0..(size * size - 1)]]
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
  hSetBuffering stdout NoBuffering
  putStr "Specify board file: "
  boardConfFile <- getLine
  configuration <- readFile boardConfFile
  case setBoard configuration of
    Left err -> print err
    Right board -> do
      play displayMode bgColor fps board drawApp handleEvent updateBoard
