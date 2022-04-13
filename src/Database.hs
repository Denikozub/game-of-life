{-# LANGUAGE OverloadedStrings #-}

module Database where

import Graphics.Gloss.Data.Color
import Database.SQLite.Simple

import Consts
import GameOfLife

data Settings = Settings {
  sFps :: Int,
  sBgColor :: Color, 
  sDeadColor :: Color,
  sAliveColor :: Color
}

{-
      Boards
id(INTEGER)  n(INTEGER)

        Cells
id(INTEGER) alivePos(INTEGER)
id REFERENCES Boards(id)

                              Settings
id(INTEGER)  fps(INTEGER) bgColor(TEXT) deadColor(TEXT) aliveColor(TEXT)
id REFERENCES Boards(id)
-}

-- Save board and its ID to Boards table
-- Also save default settings to Settings table
dbSaveBoard :: Int -> Board -> IO ()
dbSaveBoard _ _ = do
  conn <- open "test.db"
  close conn

-- Get list of IDs from Boards table
dbGetBoardIDs :: [Int]
dbGetBoardIDs = []

-- Get board by its ID from Boards table
dbGetBoard :: Int -> Board
dbGetBoard _ = Board 0 []

-- Delete row by ID from Boards table
-- Also delete corresponding row from Settings table
dbDeleteBoard :: Int -> IO ()
dbDeleteBoard _ = do
  conn <- open "test.db"
  close conn

-- Save settings and their ID to Settings table
dbSaveSettings :: Int -> Settings -> IO ()
dbSaveSettings _ _ = do
  conn <- open "test.db"
  close conn

-- Get settings by their ID from Settings table
dbGetSettings :: Int -> Settings
dbGetSettings _ = Settings fps bgColor deadColor aliveColor