{-# LANGUAGE LambdaCase #-}

module Game.State where

import System.Random
import Control.Monad
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (mapMaybe)
import Control.Applicative (asum)

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


data GameEvents = GameEvents
  { directions :: [Direction],
    pause :: Bool,
    windowResize :: Maybe (Int, Int)
  }

parseEvents :: [Event] -> GameEvents
parseEvents evs = GameEvents
  (mapMaybe getDir evs) (any getPause evs) (asum $ fmap getResize evs)
  where
    getDir :: Event -> Maybe Direction
    getDir (EventKey k Down _ _) = case k of
      Char 'w' -> Just U
      Char 's' -> Just D
      Char 'a' -> Just L
      Char 'd' -> Just R
      SpecialKey KeyUp -> Just U
      SpecialKey KeyDown -> Just D
      SpecialKey KeyLeft -> Just L
      SpecialKey KeyRight -> Just R
      _ -> Nothing
    getDir _ = Nothing

    getPause :: Event -> Bool
    getPause (EventKey (SpecialKey KeyEsc) Down _ _) = True
    getPause _ = False

    getResize :: Event -> Maybe (Int, Int)
    getResize (EventResize resize) = Just resize
    getResize _ = Nothing
