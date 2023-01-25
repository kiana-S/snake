{-# LANGUAGE Arrows #-}

module Game.Engine where

import Data.MonadicStreamFunction
import Game.State
import Game.Utils

handleEvents :: Monad m => MSF (DrawerT m) () [Direction]
handleEvents = proc () -> do
  returnA -< []

tick :: Monad m => MSF (DrawerT m) (Maybe Direction) GameState
tick = next initialState $ feedback initialState $ proc (dir, state) -> do
  returnA -< (state, state)

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
