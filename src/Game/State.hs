module Game.State where

data Direction = U | D | L | R

movePos :: Direction -> (Int, Int) -> (Int, Int)
movePos U (x, y) = (x, y - 1)
movePos D (x, y) = (x, y + 1)
movePos L (x, y) = (x - 1, y)
movePos R (x, y) = (x + 1, y)

opposite :: Direction -> Direction -> Bool
opposite U D = True
opposite D U = True
opposite L R = True
opposite R L = True
opposite _ _ = False

setDir :: Direction -> Maybe Direction -> Direction
setDir d Nothing = d
setDir d (Just d') = if opposite d d' then d else d'

data GameState = GameState
  { snakePos :: [(Int, Int)],
    moveDir :: Direction,
    berryPos :: (Int, Int)
  }

initialState :: GameState
initialState = GameState [(4, 4)] R (3, 3)
