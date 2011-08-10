module Main where

import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama    (getScreenInfo)
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Concurrent

import P
import Operations
import Controls
import Keys
import qualified WindowSplit as W

numlockMask :: KeyMask
numlockMask = mod2Mask

main :: IO ()
main = do
  dpy    <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  evmv   <- newEmptyMVar
  wmdelt <- internAtom dpy "WM_DELETE_WINDOW" False
  wmprot <- internAtom dpy "WM_PROTOCOLS"     False
  xinesc <- getScreenInfo dpy
  --nbc    <- initcolor normalBorderColor
  --fbc    <- initcolor focusedBorderColor
  --args   <- getArgs

  let cf = PConf
          { display       = dpy
          , rootWin       = rootw
          --, keyMap        = defaultKeys
          , evMVar        = evmv
          , wmDelete      = wmdelt
          , wmProtocols   = wmprot }
      st = PState
          { windowState   = W.empty xinesc
          , xineScreens   = xinesc
          , dimensions    = (fromIntegral (displayWidth  dpy dflt),
                             fromIntegral (displayHeight dpy dflt)) }

  xSetErrorHandler

  sync dpy False
  selectInput dpy rootw $  substructureRedirectMask
                       .|. substructureNotifyMask
                       .|. enterWindowMask
                       .|. leaveWindowMask
                       .|. structureNotifyMask
  grabKeys dpy rootw
  sync dpy False

  ws <- scan dpy rootw
  forkIO (allocaXEvent $ \e -> xevThread evmv dpy e)
  forkIO (pevThread evmv)
  runP cf st $ do
      mapM_ manage ws
      forever $ do --handle =<< io (nextEvent dpy e >> getEvent e)
          ev <- io (takeMVar evmv)
          handle ev
    where forever a = a >> forever a
          xevThread mv dpy e = forever $ do
              nextEvent dpy e
              ev <- getEvent e
              putMVar mv (XEv ev)
          pevThread mv = return ()
              

-- |run on startup, scans for windows to try to manage and manages them.
scan :: Display -> Window -> IO [Window]
scan dpy rootw = do
    (_, _, ws) <- queryTree dpy rootw
    filterM ok ws
  where ok w = do wa <- getWindowAttributes dpy w
                  return $ not (wa_override_redirect wa)
                         && wa_map_state wa == waIsViewable

-- |grab all toplevel keys defined in 'keys'
grabKeys :: Display -> Window -> IO ()
grabKeys dpy rootw = do
    ungrabKey dpy anyKey anyModifier rootw
    flip mapM_ (M.keys keys) $ \(mask,sym) -> do
         kc <- keysymToKeycode dpy sym
         when (kc /= 0) $ mapM_ (grab kc . (mask .|.)) $
            [0, numlockMask, lockMask, numlockMask .|. lockMask]

  where grab kc m = grabKey dpy kc m rootw True
                    grabModeAsync grabModeAsync

-- |Main event handler, maps each event onto a P ().
--  Most of them with apologies to XMonad 0.2
handle :: PEvent -> P ()

handle (XEv (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code}))
    | t == keyPress = withDisplay $ \dpy -> do
        s <- io $ keycodeToKeysym dpy code 0
        let mSane = (complement (numlockMask .|. lockMask)) .&. m
        debugP $ putStr "got key: "
        debugP $ (print (mSane,s))
        whenJust (M.lookup (mSane,s) keys) id

-- Thrown when a 
handle (XEv (MapRequestEvent {ev_window = w})) = withDisplay $
  \dpy -> do
    wa <- io $ getWindowAttributes dpy w -- ignore override windows
    when (not (wa_override_redirect wa)) $ do
      manage w
      windows $ W.manage W.Nice w

handle (XEv (DestroyWindowEvent {ev_window = w})) = whenM (isClient w) $ unmanage w
handle (XEv (UnmapEvent         {ev_window = w})) = whenM (isClient w) $ unmanage w

handle (XEv e@(MappingNotifyEvent {ev_window = w})) = do
    io $ refreshKeyboardMapping e
    when (ev_request e == mappingKeyboard) $ withDisplay $ io . flip grabKeys w

handle (XEv e@(ConfigureRequestEvent {})) = withDisplay $ \dpy -> do
    io $ configureWindow dpy (ev_window e) (ev_value_mask e) $ WindowChanges
        { wc_x            = ev_x e
        , wc_y            = ev_y e
        , wc_width        = ev_width e
        , wc_height       = ev_height e
        , wc_border_width = ev_border_width e
        , wc_sibling      = ev_above e
        , wc_stack_mode   = fromIntegral $ ev_detail e }
    io $ sync dpy False


handle (XEv (ConfigureEvent {ev_window = w})) = whenM (isRoot w) rescreen

handle _ = return () -- ignore everything except the above
