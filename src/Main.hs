{-# LANGUAGE Arrows #-}

module Main where

import Data.MonadicStreamFunction
import Graphics.Gloss
import Utils

mainSF :: MSF (DrawerT IO) () ()
mainSF = proc () -> do
  draw -< color white $ circleSolid 40

main :: IO ()
main = playMSF FullScreen black 60 (runDrawerS mainSF)
