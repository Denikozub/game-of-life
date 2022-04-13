module Interface where

import Graphics.Gloss.Interface.Pure.Game

data State = Alive | Dead deriving Eq

data Board = Board Int [State]

-- Left board of board IDs
data TableOfBoardID = TableOfBoardID [Int]

-- Right board of settings IDs
data TableOfSettingID = TableOfSettingID [Int]

-- New board, sum of main board, left board and right board
data NewBoard = NewBoard {
  board_bis :: Board,
  tableOfBoardID :: TableOfBoardID,
  tableOfSettingID :: TableOfSettingID
}

-- Draw left board
drawTableOfBoards :: [Int] -> Picture
drawTableOfBoards _ = Blank

-- Draw right board
drawTableOfSettings :: [Int] -> Picture
drawTableOfSettings _ = Blank

-- click on the left board
clickBoards :: Event -> NewBoard -> NewBoard
clickBoards _ newBoard = newBoard

-- Click on the right board
clickSettings :: Event -> NewBoard -> NewBoard
clickSettings _ newBoard = newBoard

-- Draw a picture of three boards state
drawNewApp :: NewBoard -> Picture
drawNewApp _ = Blank

-- Handle all IO events
handleNewEvent :: Event -> NewBoard -> NewBoard
handleNewEvent _ newBoard = newBoard

{-
drawNewApp :: NewBoard -> Picture
drawNewApp newBoard@(Board size _ TableOfBoardID _ TableOfSettingID _) = Pictures [
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
-}