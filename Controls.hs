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
    refresh

unsplit :: P ()
unsplit = windows W.unsplit

splitV :: P ()
splitV = windows W.splitV

-- | removes a window from our management
unmanage :: Window -> P ()
unmanage = windows . W.delete

-- | sets the input focus to the currently active window or root if none
setTopFocus :: P ()
setTopFocus = withWinState $ \s -> maybe (O.setFocus =<< asks rootWin)
                                         O.setFocus (W.peek s)

-- | repopulates the screen with the windows we want to see
refresh :: P ()
refresh = do
    debugP $ putStrLn "refresh called."
    PState { windowState = ws, xineScreens = xs } <- get
    d <- asks display

    (`mapM` (W.allLeaves ws)) $ \l -> do
        let s = W.screenid l
            w = W.window l
            e = W.extent l
            --r = genericIndex xs s

        whenJust w (\jw -> io $ do
           O.tileWindow d jw e
           raiseWindow d jw )
    setTopFocus
    O.clearEnterEvents

-- |switches focus to the next window
nextwin :: P ()
nextwin = do
    s <- gets windowState
    debugP $ putStrLn "nextwin: current state is:"
    debugP $ print s
    windows W.next

-- |switches focus to next frame
nextframe :: P ()
nextframe = do
    s <- gets windowState
    O.debug $ putStrLn "nextframe: current state is:"
    O.debug $ print s
    windows W.focusNext
