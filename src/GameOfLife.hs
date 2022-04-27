module GameOfLife where

import Graphics.Gloss.Interface.IO.Game
import Text.Read
import Indexing
import System.IO
import System.Random

import Types
import Consts
import Database


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
updateBoard :: Float -> Board -> IO Board
updateBoard _ board@(Board size cells) = do
  return $ Board size (
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
drawApp :: Board -> IO Picture
drawApp board@(Board size _) = do
  ids <- dbGetBoardIDs
  return $ Pictures [
    Pictures [
      Color (if (getState j board) == Dead then deadColor else aliveColor) $ Polygon $ toFloat
        [(upperLeftX + (j `mod` size) * squareSize, upperLeftY - (j `div` size) * squareSize),
        (upperLeftX + (j `mod` size + 1) * squareSize, upperLeftY - (j `div` size) * squareSize),
        (upperLeftX + (j `mod` size + 1)*squareSize, upperLeftY - (j `div` size + 1)*squareSize),
        (upperLeftX + (j `mod` size) * squareSize, upperLeftY - (j `div` size + 1) * squareSize)]
        | j <- [0..(size * size - 1)]], 
    Pictures [
      Color (if i `mod` 2 == 0 then buttonColor_1 else buttonColor_2) $ Polygon $ toFloat
        [(leftXAnchor, leftYAnchor - buttonSize*i),
        (leftXAnchor + buttonSize, leftYAnchor - buttonSize*i),
        (leftXAnchor + buttonSize, leftYAnchor - buttonSize*(i+1)),
        (leftXAnchor, leftYAnchor - buttonSize*(i+1))]
        | i <- [0..((length ids) - 1)]], 
    Color godModeColor $ Polygon $ toFloat
        [(rightXAnchor, leftYAnchor),
        (rightXAnchor + buttonSize, leftYAnchor),
        (rightXAnchor + buttonSize, leftYAnchor - buttonSize),
        (rightXAnchor, leftYAnchor - buttonSize)],
    Color deadColor $ Translate (fromIntegral (rightXAnchor + buttonSize + 10) :: Float) 
      (fromIntegral (leftYAnchor - buttonSize + 10) :: Float) $ Scale 0.3 0.3 (Text $ "GOD MODE"),
    Color deadColor $ Translate (fromIntegral (leftXAnchor - 4*buttonSize) :: Float) 
      (fromIntegral (leftYAnchor - buttonSize + 10) :: Float) $ Scale 0.3 0.3 (Text $ "LIST OF"),
    Color deadColor $ Translate (fromIntegral (leftXAnchor - 4*buttonSize) :: Float) 
      (fromIntegral (leftYAnchor - 2*buttonSize + 10) :: Float) $ Scale 0.3 0.3 (Text $ "BOARDS")]
    where
      leftXAnchor = - (screenSize `div` 2) - 2*buttonSize
      rightXAnchor = screenSize `div` 2 + buttonSize
      leftYAnchor = screenSize `div` 2 - 2*buttonSize
      squareSize = screenSize `div` size
      upperLeftX = - (screenSize `div` 2)
      upperLeftY = screenSize `div` 2
      toFloat :: [(Int, Int)] -> [(Float, Float)]
      toFloat [] = []
      toFloat ((a, b) : xs) = 
        (fromIntegral a :: Float, fromIntegral b :: Float) : (toFloat xs)


-- Handle IO events
handleEvent :: Event -> Board -> IO Board
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) board@(Board size cells) = do
  ids <- dbGetBoardIDs
  if (x >= (fromIntegral leftXAnchor :: Float)) && 
    (x <= (fromIntegral (leftXAnchor + buttonSize) :: Float)) &&
    (y <= (fromIntegral leftYAnchor :: Float)) &&
    (y >= (fromIntegral (leftYAnchor - (length ids)*buttonSize) :: Float)) 
  then do
    eitherNewBoard <- 
      dbGetBoard $ ids !! ((upperLeftY - 2*buttonSize - (round y)) `div` buttonSize)
    case eitherNewBoard of
      Left err -> do
        print err
        return board
      Right newBoard -> return newBoard
  else 
    if (x >= (fromIntegral rightXAnchor :: Float)) && 
      (x <= (fromIntegral (rightXAnchor + buttonSize) :: Float)) &&
      (y <= (fromIntegral leftYAnchor :: Float)) &&
      (y >= (fromIntegral (leftYAnchor - buttonSize) :: Float)) 
    then do
      rndGen <- newStdGen
      let (cell, _) = randomR (0, size * size - 1) rndGen
      return $ Board size $ map (\i -> if i == cell then (
        if (cells !! i) == Alive then Dead else Alive) else cells !! i) [0..(size * size - 1)]
    else
      return board
  where 
    leftXAnchor = - (screenSize `div` 2) - 2*buttonSize
    rightXAnchor = screenSize `div` 2 + buttonSize
    leftYAnchor = screenSize `div` 2 - 2*buttonSize
    upperLeftY = screenSize `div` 2
handleEvent _ board = do
  return board
  


run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  putStr "Specify board file: "
  boardConfFile <- getLine
  configuration <- readFile boardConfFile
  case setBoard configuration of
    Left err -> print err
    Right board -> do
      playIO displayMode bgColor fps board drawApp handleEvent updateBoard
