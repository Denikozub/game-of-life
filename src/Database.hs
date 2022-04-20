{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.SQLite.Simple

import Consts
import Types


-- Save board and its ID to Boards table
-- Also save default settings to Settings table
dbSaveBoard :: Int -> Board -> IO ()
dbSaveBoard _ _ = do
  conn <- open dbName
  close conn

-- Get list of IDs from Boards table
dbGetBoardIDs :: IO[Int]
dbGetBoardIDs = do
  conn <- open dbName
  close conn
  return []

-- Get board by its ID from Boards table
dbGetBoard :: Int -> IO Board
dbGetBoard _ = do
  conn <- open dbName
  close conn
  return $ Board 0 []

-- Delete row by ID from Boards table
-- Also delete corresponding row from Settings table
dbDeleteBoard :: Int -> IO ()
dbDeleteBoard _ = do
  conn <- open dbName
  close conn

-- Save settings and their ID to Settings table
dbSaveSettings :: Int -> Settings -> IO ()
dbSaveSettings _ _ = do
  conn <- open dbName
  close conn

-- Get settings by their ID from Settings table
dbGetSettings :: Int -> IO Settings
dbGetSettings _ = do
  conn <- open dbName
  close conn
  return $ Settings fps bgColor deadColor aliveColor