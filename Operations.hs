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
--import Controls
--import qualified WindowSplit as W

debug = io

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
    --windows $ W.manage W.Nice w

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

-- | Spawn a new process
spawn :: String -> P ()
spawn x = io $ do
    pid <- forkProcess $ do
        forkProcess (createSession >> executeFile "/bin/sh" False ["-c", x] Nothing)
        exitWith ExitSuccess
    getProcessStatus True False pid
    return ()
