{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Engine where

import Control.Monad (guard)
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

-- * Input handling

getKeys :: Monad m => MSF (DrawerT m) () [Key]
getKeys = mapMaybe getKey <$> liftTransS (constM ask)
  where
    getKey :: Event -> Maybe Key
    getKey (EventKey k Down _ _) = Just k
    getKey _ = Nothing

handleEvents :: Monad m => MSF (DrawerT m) () [Direction]
handleEvents = mapMaybe getDir <$> getKeys
  where
    getDir :: Key -> Maybe Direction
    getDir (Char 'w') = Just U
    getDir (Char 's') = Just D
    getDir (Char 'a') = Just L
    getDir (Char 'd') = Just R
    getDir (SpecialKey KeyUp) = Just U
    getDir (SpecialKey KeyDown) = Just D
    getDir (SpecialKey KeyLeft) = Just L
    getDir (SpecialKey KeyRight) = Just R
    getDir _ = Nothing

-- * Core game engine

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

gameSF :: Monad m => MSF (MaybeT (DrawerT m)) () GameState
gameSF = proc () -> do
  -- A "tick" is each frame that the snake advances
  n <- count -< ()
  let isTick = n `mod` 8 == 1

  -- Handle inputs (buffer)
  dirs <- liftTransS handleEvents -< ()
  dir <- fifoGate -< (dirs, isTick)

  -- only run `tick` whenever there's a tick
  pauseMSF undefined tick -< (dir, isTick)

mainSF :: Monad m => MSF (DrawerT m) () ()
mainSF = proc () -> do
  keys <- getKeys -< ()
  let esc = SpecialKey KeyEsc `elem` keys

  paused <- accumulateWith xor True -< esc
  state <- pauseMSF undefined (loopMaybe gameSF) -< ((), paused)

  -- Display the current state
  displayState -< state
