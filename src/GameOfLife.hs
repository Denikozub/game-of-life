module GameOfLife where

import Graphics.Gloss.Interface.Pure.Game

-- Cell state
data State = Alive | Dead

-- Board - array of cells of set size
data Board = Board {
  size :: Int,
  cells :: [State]}

-- Errors while parsing
data Error = ConfigurationError String | SizeError String

-- Display mode
display :: Display
display = FullScreen

-- Background colour
bgColor :: Color
bgColor = black

-- Update frequency
fps :: Int
fps = 1

-- Set board configuration from text
setBoard :: String -> Either Error Board
setBoard configuration = Left (ConfigurationError "Wrong configuration")

-- Get cell state by its position in array
getState :: Int -> State
getState pos = Dead

-- Get list if cell neighbours by its position in array and array size
neighbours :: Int -> Int -> [Int]
neighbours size pos = []

-- Update cell state by its current state and neighbours' states
updateCell :: State -> Int -> State
updateCell state aliveNeighbours = state

-- Update all cells on board
updateBoard :: Float -> Board -> Board
updateBoard _ board = board

-- Draw a picture of current board state
drawApp :: Board -> Picture
drawApp board = Blank

-- Handle IO events
handleEvent :: Event -> Board -> Board
handleEvent _ state = state

run :: IO ()
run = do
  boardConfFile <- getLine
  configuration <- readFile boardConfFile
  case setBoard configuration of
    Left (ConfigurationError message) -> putStrLn message
    Right board -> do
      play display bgColor fps board drawApp handleEvent updateBoard
