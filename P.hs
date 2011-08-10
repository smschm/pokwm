{-# OPTIONS -fglasgow-exts #-}
module P where

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras

import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar

import System.IO

--import Operations
import qualified WindowSplit as W

data PEvent = XEv Event | SockEv String

data PState = PState
  { windowState :: !W.WindowCtx
  , dimensions  :: !(Position,Position)
  , xineScreens :: ![Rectangle] }
  deriving (Show)

data PConf = PConf
  { display     :: Display
  , rootWin     :: !Window
  , wmDelete    :: !Atom
  , wmProtocols :: !Atom
  , evMVar      :: MVar PEvent }
 -- deriving (Show)

newtype P a = P (ReaderT PConf (StateT PState IO) a)
    deriving (Functor, Monad, MonadIO,
              MonadState PState, MonadReader PConf)

debugP = io

runP :: PConf -> PState -> P a -> IO ()
runP c st (P a) = runStateT (runReaderT a c) st >> return ()

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM a f = a >>= \b -> when b f

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

trace :: (MonadIO m) => String -> m ()
trace msg = liftIO $! do hPutStrLn stderr msg; hFlush stderr

io :: IO a -> P a
io = liftIO

-- ---------------------------------------------------------------------
-- Convenient wrappers to state -- some from xmonad 0.2

-- | Run a monad action with the current display
withDisplay :: (Display -> P a) -> P a
withDisplay f = asks display >>= f

-- | True if the given window is the root window
isRoot :: Window -> P Bool
isRoot w = liftM (w==) (asks rootWin)
