data State = Alive | Dead

data Board = Board {
  size :: Int,
  cells :: [State]
}

data Error = ConfigurationError String | SizeError String

setBoard :: String -> Either Error Board
setBoard configuration -> Left (ConfigurationError "Wrong configuration")

getState :: Int -> State
getState pos = Dead

neighbours :: Int -> Int -> [Int]
neighbours size pos = []

updateCell :: State -> Int -> State
updateCell state aliveNeighbours = state

updateBoard :: Board -> Board
updateBoard board -> board

main :: IO ()
  boardConfFile <- readLine
  configuration <- readFile boardConfFile
  case setBoard configuration of
    Left (ConfigurationError message) -> putStrLn message
    Right board -> 
