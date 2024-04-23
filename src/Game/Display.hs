{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Display where

import Control.Monad
import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.Writer
import Data.Maybe
import Data.MonadicStreamFunction
import Game.State
import Game.Utils
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

getSquare :: (Int, Int) -> Picture
getSquare (toEnum -> x, toEnum -> y) =
  translate (x * 25) (y * 25) $ rectangleSolid 25 25

displayState :: Monad m => MSF (DrawerT m) GameState ()
displayState = proc state -> do
  draw -< pictures $ color green . getSquare <$> state.snakePos
  draw -< color red $ getSquare state.berryPos

displayPause :: Monad m => MSF (DrawerT m) () ()
displayPause = proc () -> do
  draw -< color (withAlpha 0.5 black) $ rectangleSolid 5000 5000
  draw -< color white $ scale 0.5 0.5 $ text "Paused"
