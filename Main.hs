module Main where

import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama    (getScreenInfo)
import Control.Monad.Reader
import Control.Monad.State

import P
import Operations
import Controls
import qualified WindowSplit as W

modMask, numlockMask :: KeyMask
modMask     = mod4Mask
numlockMask = mod2Mask

-- |a mapping from keys and their masks to P () actions to take
keys :: M.Map (KeyMask, KeySym) (P ())
keys = M.fromList $
    [ ((modMask, xK_Return), spawn "uxterm")
    , ((modMask, xK_k), kill)
    , ((modMask, xK_s), splitV)
    , ((modMask, xK_u), unsplit)
    , ((modMask, xK_e), nextframe)
    , ((modMask, xK_Tab), nextwin) ] 


main :: IO ()
main = do
  dpy    <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
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
  allocaXEvent $ \e ->
      runP cf st $ do
          mapM_ manage ws
          forever $ handle =<< io (nextEvent dpy e >> getEvent e)
    where forever a = a >> forever a

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
handle :: Event -> P ()

handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyPress = withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        whenJust (M.lookup (complement
          (numlockMask .|. lockMask) .&. m,s) keys) id

-- Thrown when a 
handle (MapRequestEvent {ev_window = w}) = withDisplay $
  \dpy -> do
    wa <- io $ getWindowAttributes dpy w -- ignore override windows
    when (not (wa_override_redirect wa)) $ manage w

handle (DestroyWindowEvent {ev_window = w}) = whenM (isClient w) $ unmanage w
handle (UnmapEvent         {ev_window = w}) = whenM (isClient w) $ unmanage w

handle e@(MappingNotifyEvent {ev_window = w}) = do
    io $ refreshKeyboardMapping e
    when (ev_request e == mappingKeyboard) $ withDisplay $ io . flip grabKeys w

handle e@(ConfigureRequestEvent {}) = withDisplay $ \dpy -> do
    io $ configureWindow dpy (ev_window e) (ev_value_mask e) $ WindowChanges
        { wc_x            = ev_x e
        , wc_y            = ev_y e
        , wc_width        = ev_width e
        , wc_height       = ev_height e
        , wc_border_width = ev_border_width e
        , wc_sibling      = ev_above e
        , wc_stack_mode   = fromIntegral $ ev_detail e }
    io $ sync dpy False


handle (ConfigureEvent {ev_window = w}) = whenM (isRoot w) rescreen

handle _ = return () -- ignore everything except the above
