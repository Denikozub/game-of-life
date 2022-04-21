module Types where

import Graphics.Gloss.Data.Color

data State = Alive | Dead deriving (Eq, Show)

data Board = Board Int [State] deriving Show

data Error = ConfigurationError String | SizeError String | IdError String deriving Show

data Settings = Settings {
  sFps :: Int,
  sBgColor :: Color, 
  sDeadColor :: Color,
  sAliveColor :: Color
} deriving Show
