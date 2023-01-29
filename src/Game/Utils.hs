{-# LANGUAGE Arrows #-}

module Game.Utils where

import Control.Monad.Trans.MSF.Maybe
import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.Writer
import Data.IORef
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.ReactHandle
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Display an MSF outputting a picture value as a window using Gloss.
-- Note that the MSF is not passed a real-time clock.
--
-- The MSF is always called at a consistent framerate, regardless of when input
-- events are registered. It is passed a buffered list of all events that
-- occured between the last and current frames, with the first event in the
-- list being the latest.
playMSF :: Display -> Color -> Int -> MSF IO [Event] Picture -> IO ()
playMSF display color freq msf = do
  -- `react` doesn't allow inputs or outputs, so we have to use IORefs
  evRef <- newIORef []
  picRef <- newIORef blank

  handle <- reactInit (constM (readIORef evRef) >>> msf >>> arrM (writeIORef picRef))

  let -- An action to convert the world to a picture.
      toPic _ = readIORef picRef

      -- A function to handle input events.
      handleInput e _ = do
        modifyIORef evRef (e :)

      -- A function to step the world one iteration.
      stepWorld _ _ = do
        react handle
        writeIORef evRef []

  playIO display color freq () toPic handleInput stepWorld

-- * DrawerT

type DrawerT m = WriterT Picture (ReaderT [Event] m)

runDrawerS :: Monad m => MSF (DrawerT m) () () -> MSF m [Event] Picture
runDrawerS msf = arr (,()) >>> runReaderS (runWriterS msf) >>> arr fst

draw :: Monad m => MSF (DrawerT m) Picture ()
draw = arrM tell

-- * MSF convenience functions

hold :: Monad m => a -> MSF m (Maybe a) a
hold =
  mealy
    ( \x y -> case x of
        Nothing -> (y, y)
        Just x' -> (x', x')
    )

-- | Buffers and returns the elements in FIFO order, only allowing elements to
-- shift out whenever the input boolean is true.
fifoGate :: Monad m => MSF m ([a], Bool) (Maybe a)
fifoGate =
  mealy
    ( \(xs, b) ys ->
        if b
          then safeSnoc (ys ++ xs)
          else (Nothing, ys ++ xs)
    )
    []
  where
    safeSnoc [] = (Nothing, [])
    safeSnoc (x : xs) = (Just x, xs)

loopMaybe :: Monad m => MSF (MaybeT m) a b -> MSF m a b
loopMaybe msf = msf `catchMaybe` loopMaybe msf

pauseMSF :: Monad m => b -> MSF m a b -> MSF m (a, Bool) b
pauseMSF def msf = proc (x, b) -> do
  y <-
    if b
      then fmap Just msf -< x
      else returnA -< Nothing
  hold def -< y
