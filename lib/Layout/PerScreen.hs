{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Modified version of XMonad.Layout.PerScreen to instead check if the width of screen is greater than the height

module Layout.PerScreen (
    PerScreen,
    getPerScreenLayout,
    ifWider,
) where

import XMonad (
    LayoutClass (
        description,
        handleMessage,
        runLayout
    ),
    Mirror (Mirror),
    Rectangle (
        rect_height,
        rect_width
    ),
 )
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Renamed (
    Rename (Replace),
    renamed,
 )
import qualified XMonad.StackSet as W

import Data.Maybe (fromMaybe)

{- $usage
You can use this module by importing it into your ~\/.xmonad\/xmonad.hs file:

> import XMonad.Layout.PerScreen

and modifying your layoutHook as follows (for example):

> layoutHook = ifWider (Tall 1 (3/100) (1/2) ||| Full) Full

Replace any of the layouts with any arbitrarily complicated layout.
ifWider can also be used inside other layout combinators.
-}

ifWider ::
    (LayoutClass l1 a, LayoutClass l2 a) =>
    -- | layout to use when the screen is wide enough
    l1 a ->
    -- | layout to use otherwise
    l2 a ->
    PerScreen l1 l2 a
ifWider = PerScreen False

data PerScreen l1 l2 a = PerScreen Bool (l1 a) (l2 a)
    deriving (Read, Show)

-- | Construct new PerScreen values with possibly modified layouts.
mkNewPerScreenT :: PerScreen l1 l2 a -> Maybe (l1 a) -> PerScreen l1 l2 a
mkNewPerScreenT (PerScreen _ lt lf) mlt' =
    (\lt' -> PerScreen True lt' lf) $ fromMaybe lt mlt'

mkNewPerScreenF :: PerScreen l1 l2 a -> Maybe (l2 a) -> PerScreen l1 l2 a
mkNewPerScreenF (PerScreen _ lt lf) mlf' =
    PerScreen False lt $ fromMaybe lf mlf'

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (PerScreen l1 l2) a where
    runLayout (W.Workspace i p@(PerScreen _ lt lf) ms) r
        | rect_width r > rect_height r = do
            (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
            return (wrs, Just $ mkNewPerScreenT p mlt')
        | otherwise = do
            (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
            return (wrs, Just $ mkNewPerScreenF p mlt')

    handleMessage (PerScreen bool lt lf) m
        | bool =
            handleMessage lt m
                >>= maybe (return Nothing) (\nt -> return . Just $ PerScreen bool nt lf)
        | otherwise =
            handleMessage lf m
                >>= maybe (return Nothing) (return . Just . PerScreen bool lt)

    description (PerScreen True l1 _) = description l1
    description (PerScreen _ _ l2) = description l2

getPerScreenLayout ::
    (LayoutClass l a) => l a -> PerScreen l (ModifiedLayout Rename (Mirror l)) a
getPerScreenLayout l = ifWider l $ renamed [Replace $ description l] $ Mirror l
