{-# LANGUAGE OverloadedStrings #-}

module Database where

import Graphics.Gloss.Data.Color
import Database.SQLite.Simple
import qualified Data.Text as T

import Consts
import Types

strToColor :: String -> Maybe Color
strToColor "RGBA 0.0 0.0 0.0 1.0" = Just black
strToColor "RGBA 1.0 1.0 1.0 1.0" = Just white
strToColor "RGBA 1.0 0.0 0.0 1.0" = Just red
strToColor "RGBA 0.0 1.0 0.0 1.0" = Just green
strToColor "RGBA 0.0 0.0 1.0 1.0" = Just blue
strToColor "RGBA 1.0 1.0 0.0 1.0" = Just yellow
strToColor "RGBA 0.0 1.0 1.0 1.0" = Just cyan
strToColor "RGBA 1.0 0.0 1.0 1.0" = Just magenta
strToColor "RGBA 1.0 0.0 0.5 1.0" = Just rose
strToColor "RGBA 0.5 0.0 1.0 1.0" = Just violet
strToColor "RGBA 0.0 0.5 1.0 1.0" = Just azure
strToColor "RGBA 0.0 1.0 0.5 1.0" = Just aquamarine
strToColor "RGBA 0.5 1.0 0.0 1.0" = Just chartreuse
strToColor "RGBA 1.0 0.5 0.0 1.0" = Just orange
strToColor _ = Nothing


-- Save board and its ID to Boards table
-- Also save default settings to Settings table
dbSaveBoard :: Int -> Board -> IO ()
dbSaveBoard id_ (Board size cells) = do
  conn <- open dbName
  execute_ conn (Query $ T.pack $ "INSERT INTO " ++ boardTable ++ 
    " (id, size) VALUES (" ++ (show id_) ++ ", " ++ (show size) ++ ")")
  execute_ conn (Query $ T.pack $ "INSERT INTO " ++ settingsTable ++ 
    " (id, fps, bg_color, dead_color, alive_color) VALUES (" ++ (show id_) ++ ", " ++ (show fps) ++
    ", '" ++ (show bgColor) ++ "', '" ++ (show deadColor) ++ "', '" ++ (show aliveColor) ++ "')")
  sequence_ [insertCell conn i | i <- [0..(size * size - 1)], cells !! i == Alive]
  close conn
  where
    insertCell conn i = execute_ conn (Query $ T.pack $ "INSERT INTO " ++ cellsTable ++ 
      " (id, alive_pos) VALUES (" ++ (show id_) ++ ", " ++ (show i) ++ ")")

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
dbDeleteBoard id_ = do
  conn <- open dbName
  execute_ conn (Query $ T.pack $ "DELETE FROM " ++ boardTable ++ " WHERE id = " ++ (show id_))
  execute_ conn (Query $ T.pack $ "DELETE FROM " ++ cellsTable ++ " WHERE id = " ++ (show id_))
  execute_ conn (Query $ T.pack $ "DELETE FROM " ++ settingsTable ++ " WHERE id = " ++ (show id_))
  close conn

-- Save settings and their ID to Settings table
dbSaveSettings :: Int -> Settings -> IO ()
dbSaveSettings id_ (Settings fps_ bgColor_ deadColor_ aliveColor_) = do
  conn <- open dbName
  execute_ conn (Query $ T.pack $ "UPDATE " ++ settingsTable ++ " SET fps=" ++ (show fps_) ++
    ", bg_color='" ++ (show bgColor_) ++ "', dead_color='" ++ (show deadColor_) ++
    "', alive_color='" ++ (show aliveColor_) ++ "' WHERE id = " ++ (show id_))
  close conn

-- Get settings by their ID from Settings table
dbGetSettings :: Int -> IO Settings
dbGetSettings _ = do
  conn <- open dbName
  close conn
  return $ Settings fps bgColor deadColor aliveColor