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

-- ---------------------------------------------------------------------
-- Convenient wrappers to state -- some from xmonad 0.2

-- | Run a monadic action with the current WindowState
withWinState :: (W.WindowCtx -> P a) -> P a
withWinState f = gets windowState >>= f

-- | Run an action with the current window, if it's there
withCurrent :: (Window -> P ()) -> P ()
withCurrent f = do
    ws <- gets windowState
    whenJust (W.peek ws) f

-- | True if the given window is managed by us
isClient :: Window -> P Bool
isClient w = do
    debugP $ do
        putStr "calling isClient on "
        print w
    isC <- withWinState (\ws -> return (W.member w ws))
    debugP $ do
        putStr "result: "
        print isC
    return isC
