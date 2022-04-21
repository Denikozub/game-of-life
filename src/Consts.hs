module Consts where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

displayMode :: Display
displayMode = FullScreen

bgColor, deadColor, aliveColor, rightTableColor, leftTableColor :: Color
bgColor = azure
deadColor = black
aliveColor = white
rightTableColor = yellow
leftTableColor = green

textShift :: Float
textShift = 20


fps, screenSize :: Int
fps = 1
screenSize = 1000

emptyBoardMsg, dimErrorMsg, dimRestrMsg,
  lineCountMsg, lineStateMsg, boardIdMsg, settingsIdMsg :: String
emptyBoardMsg = "Board file is empty"
dimErrorMsg = "Size required on line 1"
dimRestrMsg = "Size must be >= 3"
lineCountMsg = "First line should be followed up with size^2 lines"
lineStateMsg = "Each line after #1 should just contain 0 or 1"
boardIdMsg = "Board with such id does not exist"
settingsIdMsg = "Settings with such id do not exist"

dbName, boardTable, cellsTable, settingsTable :: String
dbName = "sqlite.db"
boardTable = "boards"
cellsTable = "cells"
settingsTable = "settings"
