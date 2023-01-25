module Game.Utils where

import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.Writer
import Data.IORef
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.ReactHandle
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Display an MSF outputting a picture value as a window using Gloss.
-- Note that the MSF is not passed a real-time clock.
playMSF :: Display -> Color -> Int -> MSF IO (Maybe Event) Picture -> IO ()
playMSF display color freq msf = do
  -- `react` doesn't allow inputs or outputs, so we have to use IORefs
  evRef <- newIORef Nothing
  picRef <- newIORef blank

  handle <- reactInit (constM (readIORef evRef) >>> msf >>> arrM (writeIORef picRef))

  let -- An action to convert the world to a picture.
      toPic _ = readIORef picRef

      -- A function to handle input events.
      handleInput e _ = do
        writeIORef evRef (Just e)

      -- A function to step the world one iteration.
      stepWorld _ _ = do
        react handle
        writeIORef evRef Nothing

  playIO display color freq () toPic handleInput stepWorld

-- * DrawerT

type DrawerT m = WriterT Picture (Control.Monad.Trans.MSF.Reader.ReaderT (Maybe Event) m)

runDrawerS :: Monad m => MSF (DrawerT m) () () -> MSF m (Maybe Event) Picture
runDrawerS msf = arr (,()) >>> Control.Monad.Trans.MSF.Reader.runReaderS (runWriterS msf) >>> arr fst

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
