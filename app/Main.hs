module Main where

import System.Environment
import Text.Read
import GameOfLife
import Database

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--upload", idStr, filename] -> do
      case readMaybe idStr :: Maybe Int of
        Nothing -> putStrLn "Wrong id: should be integer"
        Just id_ -> do
          configuration <- readFile filename
          case setBoard configuration of
            Left err -> print err
            Right board -> do
              dbSaveBoard id_ board
    ["--delete", idStr] -> do
      case readMaybe idStr :: Maybe Int of
        Nothing -> putStrLn "Wrong id: should be integer"
        Just id_ -> dbDeleteBoard id_
    [] -> run
    _ -> putStrLn "Wrong command line arguments: try --upload or --delete"
