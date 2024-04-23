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

getSquare :: (Int, Int) -> (Int, Int) -> Picture
getSquare (toEnum -> winWidth, toEnum -> winHeight) (toEnum -> x, toEnum -> y) =
  translate (x * winWidth / 30) (y * winHeight / 20) $ rectangleSolid (winWidth / 30) (winHeight / 20)

displayState :: Monad m => MSF (DrawerT m) (GameState, (Int, Int)) ()
displayState = proc (state, windowSize) -> do
  draw -< pictures $ color green . getSquare windowSize <$> state.snakePos
  draw -< color red $ getSquare windowSize state.berryPos

displayPause :: Monad m => MSF (DrawerT m) (Int, Int) ()
displayPause = proc (width, height) -> do
  draw -< color (withAlpha 0.5 black) $ rectangleSolid (toEnum width) (toEnum height)
  draw -< color white $ scale 0.5 0.5 $ text "Paused"
