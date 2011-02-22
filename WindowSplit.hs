module WindowSplit where

-- todo: documentate!

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama    (getScreenInfo)

data BTree w r s = Branch
                   { splits :: [BTree w r s]
                   , extent :: !r
                   }
               | Leaf
                   { window   :: !(Maybe w)
                   , extent   :: !r
                   , screenid :: !s
                   }

instance (Show w, Show r, Show s) => Show (BTree w r s) where
    show (Branch spl ex) = "[" ++ show ex ++ "]\n" ++ unlines (map (indent . show) spl)
      where indent = unlines . (map ('\t':)) . lines
    show (Leaf w ex sid) = "+ " ++ show w ++ " [" ++ show ex ++ ", " ++ show sid ++ "]"

data BCtx w r s = Ctx
  { left  :: [BTree w r s]
  , right :: [BTree w r s]
  , up    :: Maybe (BCtx w r s, r)
  }

instance (Show w, Show r, Show s) => Show (BCtx w r s) where
    show (Ctx l r u) = "Left:\n" ++ show l ++ "\nRight:\n" ++ show r ++ maybe "\nNo parent." (\u' -> "\nUp:\n" ++ show u') u

data Rudeness = Rude | Nice

data SplitCtx w r s = SplitCtx
  { focus   :: BTree w r s -- should always be a leaf
  , context :: BCtx w r s
  , hidden  :: [w]
  } deriving (Show)

type WindowCtx = SplitCtx Window Rectangle Int

empty :: [Rectangle] -> WindowCtx
empty xine = SplitCtx
    { focus   = f
    , context = Ctx
        { left  = []
        , right = r
        , up    = Nothing }
    , hidden  = [] }
  where (f:r) = [Leaf Nothing rect sid | (rect,sid) <- zip xine [0..]]

-- | Determines whether a given Window is in a WindowState,
--   but generalized.

isLeaf :: BTree w r s -> Bool
isLeaf (Leaf _ _ _) = True
isLeaf _            = False

allTreesCtx :: BCtx w r s -> ([BTree w r s], [BTree w r s])
allTreesCtx (Ctx l r u) = case u of
    Nothing  -> (l, r)
    Just (ctx, _) -> let (l', r') = allTreesCtx ctx in (l' ++ l, r ++ r')

allTrees :: SplitCtx w r s -> [BTree w r s]
allTrees (SplitCtx f c _) = let (l',r') = allTreesCtx c in l' ++ [f] ++ r'

allLeaves :: SplitCtx w r s -> [BTree w r s]
allLeaves = (filter isLeaf) . allTrees

{-
allWindows :: BCtx w r s -> [w]
allWindows (Ctx f l r u h) = (allWindowsInTree =<< (f : (l ++ r)))
                          ++ maybe [] allWindows u ++ h
-}

allWindows :: SplitCtx w r s -> [w]
allWindows c = ((allTrees c) >>= allWindowsInTree) ++ (hidden c)

allWindowsInTree :: BTree w r s -> [w]
allWindowsInTree t = case t of
    Leaf (Just w') _ _  -> [w']
    Leaf Nothing _ _   -> []
    Branch s _         -> allWindowsInTree =<< s

member :: (Show w, Eq w) => w -> SplitCtx w r s -> Bool
member w c = case (focus c) of
    Leaf (Just w') _ _ -> w == w' -- common case
    _                  -> w `elem` (allWindows c)

with :: a -> (w -> a) -> SplitCtx w r s -> a
with d f c = maybe d f (peek c)

peek :: SplitCtx w r s -> Maybe w
peek c = case (focus c) of
    Leaf (Just w') _ _ -> Just w'
    _                  -> Nothing

delete :: (Eq w) => w -> SplitCtx w r s -> SplitCtx w r s
delete w (SplitCtx f c h) = SplitCtx f' c' h'
  where f' = deleteFromTree w f
        c' = deleteFromCtx w c
        h' = filter (/= w) h

deleteFromCtx :: (Eq w) => w -> BCtx w r s -> BCtx w r s
deleteFromCtx w (Ctx l r u) = Ctx l' r' u'
  where l' = map (deleteFromTree w) l
        r' = map (deleteFromTree w) r
        u' = maybe Nothing (Just . (\(uctx,uex) -> (deleteFromCtx w uctx,uex))) u

deleteFromTree :: (Eq w) => w -> BTree w r s -> BTree w r s
deleteFromTree w (Branch s e) = Branch (map (deleteFromTree w) s) e
deleteFromTree w leaf = if window leaf == (Just w) then leaf { window = Nothing }
                                                   else leaf

manage :: Rudeness -> w -> SplitCtx w r s -> SplitCtx w r s
manage Nice w c = c { hidden = w : (hidden c) }
manage Rude w c = case (focus c) of
    Leaf (Just cw) ex sid -> c { focus = Leaf (Just w) ex sid
                               , hidden = cw : (hidden c) }
    Leaf Nothing ex sid   -> c { focus = Leaf (Just w) ex sid }
    _                     -> error "A non-leaf is focused! Why?"

next :: SplitCtx w r s -> SplitCtx w r s
next c = case (focus c, hidden c) of
    (Leaf (Just cw) ex sid, (hw:hws)) ->
        c { focus = Leaf (Just hw) ex sid, hidden = hws ++ [cw] }
    (Leaf Nothing ex sid, (hw:hws))   ->
        c { focus = Leaf (Just hw) ex sid, hidden = hws }
    _                                 -> c


unsplit :: WindowCtx -> WindowCtx
unsplit c@(SplitCtx _ (Ctx _ _ Nothing) _) = c
unsplit c = SplitCtx (f {extent = uex}) uctx h'
  where (SplitCtx f (Ctx l r (Just (uctx, uex))) h) = c
        h' = (allWindowsInTree =<< l) ++ (allWindowsInTree =<< r) ++ h

split :: (r -> [r]) -> SplitCtx w r s -> SplitCtx w r s
split spf c = SplitCtx
    { focus = f'
    , context = Ctx
        { left = []
        , right = fr'
        , up = Just (ctx, ex) }
    , hidden = h }
  where (SplitCtx f ctx h) = c
        (Leaf cw ex sid) = f
        (ex0:exes) = spf ex
        f' = Leaf cw ex0 sid
        fr' = [Leaf Nothing ex' sid | ex' <- exes]


focusNext :: SplitCtx w r s -> SplitCtx w r s
focusNext c = let (t', c') = focusNextCtx (focus c) (context c)
              in  c { focus = t', context = c' }


--------------- current focus, current context, new focus,new context
focusNextCtx :: BTree w r s -> BCtx w r s -> (BTree w r s, BCtx w r s)
focusNextCtx ct c@(Ctx l r u) = case (isLeaf ct, l, r, u) of
    -- Must be only one window?
    (True, [], [], Nothing)  -> (ct,     c)
    -- Windows to the right in this subtree
    (True, l,  (n:ns), u)    -> (n,      Ctx (ct:l) ns u)
    -- No windows to the right, at the top of the tree
    (True, l,  [], Nothing)  -> (last l, Ctx (reverse (init l)) [] Nothing)
    -- No windows to the right, not at the top
    (True, l,  [], Just (uctx, uex)) ->
        let tree' = Branch (reverse (ct:l)) uex
        in  focusNextCtx tree' uctx
    -- Non-leaf, at the top of the tree
    -- Q: Should this happen? A: yes, at the top with one screen
    (False, [], [], Nothing) -> descend ct c
    -- Non-leaf with elements to the right, we don't need to go up, just focus
    -- downward into the next right-most btree
    (False, l, (n:ns), u)    ->
        let c' = Ctx (ct:l) ns u
        in  descend n c'
    (False, l, [], Nothing)  ->
        let c' = Ctx (reverse (init l)) [] Nothing
            t' = last l
        in  descend t' c'
    (False, l, [], Just (uctx, uex)) -> --fuck
        let tree' = Branch (reverse (ct:l)) uex
        in  focusNextCtx tree' uctx

----- descend into a tree given a current context
---------- descend into,  current context,  new leaf,    new context
descend :: BTree w r s -> BCtx w r s -> (BTree w r s, BCtx w r s)
descend t c
    | isLeaf t  = (t, c)
    | otherwise = descend (head spl) (Ctx [] (tail spl) (Just (c,bex)))
        where (Ctx cl cr cu)   = c
              (Branch spl bex) = t

fi x = fromIntegral x

rsplitV :: Rectangle -> [Rectangle]
rsplitV (Rectangle rx ry rw rh) =
    [Rectangle rx ry rw0 rh, Rectangle rx1 ry rw1 rh]
  where rw0 = (fi rw) `div` 2
        rx1 = (fi rx) + (fi rw0)
        rw1 = rw - rw0

splitV :: WindowCtx -> WindowCtx
splitV = split rsplitV
