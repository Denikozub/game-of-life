module GameOfLife where

import System.Environment
import Data.Char
import Graphics.Gloss.Interface.Pure.Game

data State = Dead | Alive

data Ceil = Ceil 
  {
	x :: Int,
	y :: Int,
    state :: State
  }

data Field = Field [[Ceil]]

data AppState = AppState
  { 
    field :: Field, -- Current field.
    step :: Int -- Current iteration.
  }

-- Кол-во живых соседей. На вход подаются поле и координаты клетки 
countNeighbors :: Ceil -> Field -> Int
countNeighbors _ _ = 0

-- Следующая итерация игры. Для каждой клетки посчитать кол-во живых соседей и перерисовать поле
nextIter :: Field -> Field
nextIter f = f

-- Отрисовка поля???
-- printField :: Field -> 

-- Чтобы не падать по пустякам
checkFilename :: String -> Bool
checkFilename _ = True

-- Считывание поля из строк, считанных из файла
createField :: Int -> [[String]] -> Field
createField _ _ = Field [[Ceil 0 0 Dead]]

display :: Display
display = FullScreen