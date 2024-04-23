{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.Engine where

import Control.Monad (guard)
import Control.Monad.Trans
import Control.Monad.Trans.MSF.Maybe
import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.Writer
import Data.Bits (xor)
import Data.Functor (($>))
import Data.Maybe
import Data.MonadicStreamFunction
import Game.Display
import Game.State
import Game.Utils
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Applicative (asum)

-- * Input handling

handleEvents :: Monad m => MSF (DrawerT m) () GameEvents
handleEvents = parseEvents <$> liftTransS (constM ask)

-- * Core game engine

tickState :: Maybe Direction -> GameState -> MaybeT IO GameState
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
          then randomPos
          else pure state.berryPos
   in MaybeT $ (guard isHit $>) <$> (GameState snakePos moveDir <$> berryPos)

tick :: MonadIO m => MSF (MaybeT (DrawerT m)) (Maybe Direction) GameState
tick = feedbackM (liftIO randomState) $ proc (dir, state) -> do
  newstate <- arrM (mapMaybeT (lift . lift . liftIO) . uncurry tickState) -< (dir, state)
  returnA -< (newstate, newstate)

gameSF :: MonadIO m => MSF (MaybeT (DrawerT m)) GameEvents GameState
gameSF = proc events -> do
  -- A "tick" is each frame that the snake advances
  n <- count -< ()
  let isTick = n `mod` 8 == 1

  -- Handle inputs (buffer)
  dir <- fifoGate -< (events.directions, isTick)

  -- only run `tick` whenever there's a tick
  pauseMSF undefined tick -< (dir, isTick)

mainSF :: MonadIO m => (Int, Int) -> MSF (DrawerT m) () ()
mainSF initSize = proc () -> do
  events <- handleEvents -< ()

  -- Handle window resize
  windowSize <- hold initSize -< events.windowResize

  unpaused <- accumulateWith xor True -< events.pause
  state <- pauseMSF undefined (loopMaybe gameSF) -< (events, unpaused)

  -- Display the current state
  displayState -< (state, windowSize)
  if unpaused
    then returnA -< ()
    else displayPause -< windowSize
