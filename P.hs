{-# OPTIONS -fglasgow-exts #-}
module P where

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras

import Control.Monad.Reader
import Control.Monad.State

import System.IO

--import Operations
import qualified WindowSplit as W

data PState = PState
  { windowState :: !W.WindowCtx
  , dimensions  :: !(Position,Position)
  , xineScreens :: ![Rectangle] }
  deriving (Show)

data PConf = PConf
  { display     :: Display
  , rootWin     :: !Window
  , wmDelete    :: !Atom
  , wmProtocols :: !Atom }
  deriving (Show)

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
    debugP $ do
        putStr "calling isClient on "
        print w
    isC <- withWinState (\ws -> return (W.member w ws))
    debugP $ do
        putStr "result: "
        print isC
    return isC
