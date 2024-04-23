module Main where

import Data.MonadicStreamFunction
import Game.Engine
import Game.Utils
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
  size <- getScreenSize
  playMSF FullScreen black 60 (runDrawerS $ mainSF size)
