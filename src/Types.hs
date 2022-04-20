module Types where

import Graphics.Gloss.Data.Color

data State = Alive | Dead deriving Eq

data Board = Board Int [State]

data Error = ConfigurationError String | SizeError String

data Settings = Settings {
  sFps :: Int,
  sBgColor :: Color, 
  sDeadColor :: Color,
  sAliveColor :: Color
}
