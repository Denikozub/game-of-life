{-# LANGUAGE OverloadedStrings #-}

module Database where

import Graphics.Gloss.Data.Color
import Database.SQLite.Simple
import qualified Data.Text as T

import Consts
import Types


{-
CREATE TABLE boards (id INTEGER PRIMARY KEY, size INTEGER)
CREATE TABLE cells (id INTEGER REFERENCES boards(id), alive_pos INTEGER)
CREATE TABLE settings (id INTEGER PRIMARY KEY REFERENCES boards(id),
                       fps INTEGER,
                       bg_color Text,
                       dead_color text,
                       alive_color Text)
-}

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
  ids <- query_ conn (Query $ T.pack $ "SELECT id FROM " ++ boardTable) :: IO [Only Int]
  close conn
  return $ map fromOnly ids

-- Get board by its ID from Boards table
dbGetBoard :: Int -> IO (Either Types.Error Board)
dbGetBoard id_ = do
  conn <- open dbName
  sizeOnly <- query_ conn (Query $ T.pack $ "SELECT size FROM " ++
    boardTable ++ " WHERE id = " ++ (show id_)) :: IO [Only Int]
  case sizeOnly of
    [] -> return $ Left $ IdError boardIdMsg
    (x:_) -> do
      let size = fromOnly x
      aliveCellsOnly <- query_ conn (Query $ T.pack $ "SELECT alive_pos FROM " ++
        cellsTable ++ " WHERE id = " ++ (show id_)) :: IO [Only Int]
      let aliveCells = (map fromOnly aliveCellsOnly)
      let cells = [if elem i aliveCells then Alive else Dead | i <- [0..(size * size - 1)]]
      close conn
      return $ Right $ Board size cells

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

strToColor :: String -> Color
strToColor "RGBA 0.0 0.0 0.0 1.0" = black
strToColor "RGBA 1.0 1.0 1.0 1.0" = white
strToColor "RGBA 1.0 0.0 0.0 1.0" = red
strToColor "RGBA 0.0 1.0 0.0 1.0" = green
strToColor "RGBA 0.0 0.0 1.0 1.0" = blue
strToColor "RGBA 1.0 1.0 0.0 1.0" = yellow
strToColor "RGBA 0.0 1.0 1.0 1.0" = cyan
strToColor "RGBA 1.0 0.0 1.0 1.0" = magenta
strToColor "RGBA 1.0 0.0 0.5 1.0" = rose
strToColor "RGBA 0.5 0.0 1.0 1.0" = violet
strToColor "RGBA 0.0 0.5 1.0 1.0" = azure
strToColor "RGBA 0.0 1.0 0.5 1.0" = aquamarine
strToColor "RGBA 0.5 1.0 0.0 1.0" = chartreuse
strToColor _ = orange

-- Get settings by their ID from Settings table
dbGetSettings :: Int -> IO (Either Types.Error Settings)
dbGetSettings id_ = do
  conn <- open dbName
  settings <- query_ conn (Query $ T.pack $ "SELECT fps, bg_color, dead_color, alive_color FROM "
    ++ settingsTable ++ " WHERE id = " ++ (show id_)) :: IO [(Int, String, String, String)]
  case settings of
    [] -> return $ Left $ IdError settingsIdMsg
    ((fps_, bgColor_, deadColor_, aliveColor_) : _) -> do
      close conn
      return $ Right $ 
        Settings fps_ (strToColor bgColor_) (strToColor deadColor_) (strToColor aliveColor_)
