module Keys where

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras

import qualified Data.Map as M
import qualified Controls as C
import qualified Operations as O
import P

modMask :: KeyMask
modMask     = mod4Mask

-- |a mapping from keys and their masks to P () actions to take
keys :: M.Map (KeyMask, KeySym) (P ())
keys = M.fromList $
    [ ((modMask, xK_Return), O.spawn "uxterm")
    , ((modMask, xK_k), C.killCurrent)
    , ((modMask, xK_s), C.splitV)
    , ((modMask, xK_u), C.unsplit)
    , ((modMask, xK_e), C.nextframe)
    , ((modMask, xK_Tab), C.nextwin) ] 
