module Operations where

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama    (getScreenInfo)

import Data.Bits
import Data.Maybe
import Data.List            (genericIndex, intersectBy)
import Control.Monad.Reader
import Control.Monad.State
import System.Posix.Process (executeFile, forkProcess, getProcessStatus, createSession)
import System.Exit

import P
import qualified WindowSplit as W

debug = io

-- | lifts a pure function that mutates a WindowState into the P monad
windows :: (W.WindowCtx -> W.WindowCtx) -> P ()
windows f = do
    old  <- gets windowState
    let new = f old
    modify (\s -> s { windowState = new })
    refresh

-- | puts a new window under our management
manage :: Window -> P ()
manage w = do
    debug $ do
        putStr "manage: managing window "
        print w
    withDisplay $ \d -> io $ do
        selectInput d w $ structureNotifyMask
                      .|. enterWindowMask
                      .|. propertyChangeMask
        mapWindow d w
        --setWindowBorderWidth d w borderWidth
    windows $ W.manage W.Nice w

-- | removes a window from our management
unmanage :: Window -> P ()
unmanage = windows . W.delete

unsplit :: P ()
unsplit = windows W.unsplit

splitV :: P ()
splitV = windows W.splitV

-- | kills a specific window
kill :: P ()
kill = withDisplay $ \d -> withCurrent $ \w -> do
    PConf {wmDelete = wmdelt, wmProtocols = wmprot} <- ask
    protocols <- io $ getWMProtocols d w
    io $ if wmdelt `elem` protocols
        then allocaXEvent $ \ev -> do
                setEventType ev clientMessage
                setClientMessageEvent ev w wmprot 32 wmdelt 0
                sendEvent d w False noEventMask ev
        else killClient d w >> return ()

-- | called on rescreen event: this is currently unhandled for now!
rescreen :: P () -- TODO: make this a little bit more graceful
rescreen = do
    io $ putStrLn "rescreen event currently unhandled."
    return ()

-- | repopulates the screen with the windows we want to see
refresh :: P ()
refresh = do
    debug $ putStrLn "refresh called."
    PState { windowState = ws, xineScreens = xs } <- get
    d <- asks display

    (`mapM` (W.allLeaves ws)) $ \l -> do
        let s = W.screenid l
            w = W.window l
            e = W.extent l
            --r = genericIndex xs s

        whenJust w (\jw -> io $ do
           tileWindow d jw e
           raiseWindow d jw )
    setTopFocus
    clearEnterEvents


-- | sets the input focus to the currently active window or root if none
setTopFocus :: P ()
setTopFocus = withWinState $ \s -> maybe (setFocus =<< asks rootWin)
                                         setFocus (W.peek s)

-- | sets the focus to a specific window
setFocus :: Window -> P ()
setFocus w = withWinState $ \ws -> do
    PConf { display = dpy } <- ask

    -- clear mouse button grab and border on other windows
    {-
    (`mapM_` (current ws : visible ws)) $ \wk -> do
        (`mapM_` (W.index (W.view (W.tag (W.workspace wk)) ws))) $ \otherw -> do
            setButtonGrab True otherw
            io $ setWindowBorder dpy otherw (color_pixel nbc)
    -}
    io $ do setInputFocus dpy w revertToPointerRoot 0
            -- raiseWindow dpy w
    --setButtonGrab False w
    --io $ setWindowBorder dpy w (color_pixel fbc)

clearEnterEvents :: P ()
clearEnterEvents = withDisplay $ \d -> io $ do
    sync d False
    allocaXEvent $ \p -> fix $ \again -> do
        more <- checkMaskEvent d enterWindowMask p
        when more again -- beautiful

-- |puts a window into a rectangle on a display
tileWindow :: Display -> Window -> Rectangle -> IO ()
tileWindow d w r = do
    bw <- (fromIntegral . wa_border_width) `liftM` getWindowAttributes d w
    moveResizeWindow d w (rect_x r) (rect_y r)
                         (rect_width  r - bw*2) (rect_height r - bw*2)

-- |switches focus to the next window
nextwin :: P ()
nextwin = do
    s <- gets windowState
    debug $ putStrLn "nextwin: current state is:"
    debug $ print s
    windows W.next

-- |switches focus to next frame
nextframe :: P ()
nextframe = do
    s <- gets windowState
    debug $ putStrLn "nextframe: current state is:"
    debug $ print s
    windows W.focusNext
-- ---------------------------------------------------------------------
-- Convenient wrappers to state -- some from xmonad 0.2

-- | Run a monad action with the current display
withDisplay :: (Display -> P a) -> P a
withDisplay f = asks display >>= f

-- | Run a monadic action with the current WindowState
withWinState :: (W.WindowCtx -> P a) -> P a
withWinState f = gets windowState >>= f

-- | Run an action with the current window, if it's there
withCurrent :: (Window -> P ()) -> P ()
withCurrent f = do
    ws <- gets windowState
    whenJust (W.peek ws) f

-- | True if the given window is the root window
isRoot :: Window -> P Bool
isRoot w = liftM (w==) (asks rootWin)

-- | True if the given window is managed by us
isClient :: Window -> P Bool
isClient w = do
    debug $ do
        putStr "calling isClient on "
        print w
    isC <- withWinState (\ws -> return (W.member w ws))
    debug $ do
        putStr "result: "
        print isC
    return isC

-- | Spawn a new process
spawn :: String -> P ()
spawn x = io $ do
    pid <- forkProcess $ do
        forkProcess (createSession >> executeFile "/bin/sh" False ["-c", x] Nothing)
        exitWith ExitSuccess
    getProcessStatus True False pid
    return ()
