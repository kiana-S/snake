{-# LANGUAGE LambdaCase #-}

module Game.State where

import System.Random
import Control.Monad

data Direction = U | D | L | R deriving (Eq)

movePos :: Direction -> (Int, Int) -> (Int, Int)
movePos U (x, y) = (x, y + 1)
movePos D (x, y) = (x, y - 1)
movePos L (x, y) = (x - 1, y)
movePos R (x, y) = (x + 1, y)

opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

setDir :: Direction -> Maybe Direction -> Direction
setDir d Nothing = d
setDir d (Just d') = if opposite d == d' then d else d'

data GameState = GameState
  { snakePos :: [(Int, Int)],
    moveDir :: Direction,
    berryPos :: (Int, Int)
  }

initialLength :: Int
initialLength = 3

randomPos :: IO (Int, Int)
randomPos = (,) <$> randomRIO (-10, 10) <*> randomRIO (-10, 10)

randomState :: IO GameState
randomState = do
  let randomDirection =
        (\case
          1 -> U
          2 -> D
          3 -> L
          4 -> R) <$> randomRIO @Int (1,4)
  dirs <- replicateM (initialLength - 1) randomDirection
  hd <- randomPos
  let snake = scanr movePos hd dirs
  GameState snake <$> randomDirection <*> randomPos
