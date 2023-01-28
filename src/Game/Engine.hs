{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Engine where

import Control.Monad.Trans.MSF.Maybe
import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.Writer
import Data.Maybe
import Data.MonadicStreamFunction
import Game.Display
import Game.State
import Game.Utils
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

handleEvents :: Monad m => MSF (DrawerT m) () [Direction]
handleEvents = mapMaybe getDir <$> liftTransS (constM ask)
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


tickState :: Maybe Direction -> GameState -> Maybe GameState
tickState dir state =
  let moveDir = setDir state.moveDir dir
      newBlock = movePos moveDir $ head state.snakePos
      hitBerry = newBlock == state.berryPos
      snakePos =
        newBlock
          : if hitBerry
            then state.snakePos
            else init state.snakePos
      isHit = head snakePos `notElem` tail snakePos
      berryPos =
        if hitBerry
          then
            ( (fst state.berryPos * 80 `div` 3) `mod` 15,
              (snd state.berryPos * 75 `div` 6) `mod` 15
            )
          else state.berryPos
   in guard isHit $> GameState {..}

tick :: Monad m => MSF (MaybeT (DrawerT m)) (Maybe Direction) GameState
tick = feedback initialState $ proc (dir, state) -> do
  newstate <- maybeExit -< tickState dir state
  returnA -< (newstate, newstate)

mainSF :: Monad m => MSF (MaybeT (DrawerT m)) () ()
mainSF = proc () -> do
  -- A "tick" is each frame that the snake advances
  n <- count -< ()
  let isTick = n `mod` 8 == 1

  -- Handle inputs (buffer)
  dirs <- liftTransS handleEvents -< ()
  dir <- fifoGate -< (dirs, isTick)

  state' <-
    if isTick
      then fmap Just tick -< dir
      else returnA -< Nothing
  -- undefined is safe here because the first frame is guaranteed to be a tick
  state <- hold undefined -< state'

  -- Display the current state
  liftTransS displayState -< state
