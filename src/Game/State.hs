module Game.State where

data Direction = U | D | L | R
  deriving (Show, Eq)

movePos :: Direction -> (Int, Int) -> (Int, Int)
movePos U (x, y) = (x, y - 1)
movePos D (x, y) = (x, y + 1)
movePos L (x, y) = (x - 1, y)
movePos R (x, y) = (x + 1, y)

data GameState = GameState
  { snakePos :: [(Int, Int)],
    moveDir :: Direction,
    berryPos :: (Int, Int)
  }

initialState :: GameState
initialState = GameState [(4, 4)] R (3, 3)
