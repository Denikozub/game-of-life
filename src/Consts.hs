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