{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Engine where

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

tick :: Monad m => MSF (DrawerT m) (Maybe Direction) GameState
tick =
  next initialState $
    mealy
      ( \input state ->
          let moveDir = setDir state.moveDir input
              snakePos = movePos moveDir <$> state.snakePos
              berryPos = state.berryPos
              newstate = GameState {..}
           in (newstate, newstate)
      )
      initialState

mainSF :: Monad m => MSF (DrawerT m) () ()
mainSF = proc () -> do
  n <- count -< ()
  let isTick = n `mod` 20 == 1

  -- handle inputs (buffer)
  dirs <- handleEvents -< ()
  dir <- fifoGate -< (dirs, isTick)

  state' <-
    if isTick
      then fmap Just tick -< dir
      else returnA -< Nothing
  -- undefined is safe here because the first frame is guaranteed to be a tick
  state <- hold undefined -< state'

  returnA -< ()
