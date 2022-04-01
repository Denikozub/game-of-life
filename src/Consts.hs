module Consts where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

displayMode :: Display
displayMode = FullScreen

bgColor, deadColor, aliveColor :: Color
bgColor = azure
deadColor = black
aliveColor = white

fps, screenSize :: Int
fps = 1
screenSize = 1000

dimErrorMsg, dimRestrMsg, lineCountMsg, lineStateMsg :: String
dimErrorMsg = "Size required on line 1"
dimRestrMsg = "Size must be >= 3"
lineCountMsg = "First line should be followed up with size^2 lines"
lineStateMsg = "Each line after #1 should just contain 0 or 1"