module Utils where

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
        react handle

      -- A function to step the world one iteration.
      stepWorld _ _ = do
        writeIORef evRef Nothing
        react handle

  playIO display color freq () toPic handleInput stepWorld

type Drawer = WriterT Picture (ReaderT (Maybe Event) IO)

runDrawerS :: MSF Drawer () () -> MSF IO (Maybe Event) Picture
runDrawerS msf = arr (,()) >>> runReaderS (runWriterS msf) >>> arr fst

draw :: MSF Drawer Picture ()
draw = arrM tell
