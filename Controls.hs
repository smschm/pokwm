module Controls where

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)

import Control.Monad.Reader
import Control.Monad.State

import P
import qualified WindowSplit as W
import qualified Operations as O

-- | lifts a pure function that mutates a WindowState into the P monad
windows :: (W.WindowCtx -> W.WindowCtx) -> P ()
windows f = do
    old  <- gets windowState
    let new = f old
    modify (\s -> s { windowState = new })
    O.refresh

unsplit :: P ()
unsplit = windows W.unsplit

splitV :: P ()
splitV = windows W.splitV

-- | removes a window from our management
unmanage :: Window -> P ()
unmanage = windows . W.delete

-- | sets the input focus to the currently active window or root if none
setTopFocus :: P ()
setTopFocus = O.withWinState $ \s -> maybe (O.setFocus =<< asks rootWin)
                                         O.setFocus (W.peek s)

-- |switches focus to the next window
nextwin :: P ()
nextwin = do
    s <- gets windowState
    O.debug $ putStrLn "nextwin: current state is:"
    O.debug $ print s
    windows W.next

-- |switches focus to next frame
nextframe :: P ()
nextframe = do
    s <- gets windowState
    O.debug $ putStrLn "nextframe: current state is:"
    O.debug $ print s
    windows W.focusNext
