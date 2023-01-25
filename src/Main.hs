module Main where

import Data.MonadicStreamFunction
import Game.Engine
import Game.Utils
import Graphics.Gloss

main :: IO ()
main = playMSF FullScreen black 60 (runDrawerS mainSF)
