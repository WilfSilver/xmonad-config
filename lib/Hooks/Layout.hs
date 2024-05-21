module Hooks.Layout (
    myLayoutHook,
) where

import XMonad ((|||))
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.MultiToggle (
    EOT (EOT),
    mkToggle,
    (??),
 )
import XMonad.Layout.MultiToggle.Instances (
    StdTransformers (
        NBFULL,
        NOBORDERS
    ),
 )
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (
    onWorkspace,
    onWorkspaces,
 )
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.WindowArranger (windowArrange)

import Layout (
    floats,
    magnify,
    monocle,
    tabs,
    tall,
 )
import Layout.PerScreen (getPerScreenLayout)
import qualified Settings as S

myLayoutHook c =
    avoidStruts
        $ mouseResize
        $ windowArrange
        $ toggleLayouts floats
        $ onWorkspaces
            [S.getWs 0 c, S.getWs 4 c]
            (noBorders monocle ||| getPerScreenLayout tall)
        $ onWorkspace
            (S.getWs 1 c)
            ( getPerScreenLayout (noBorders magnify)
                ||| getPerScreenLayout (noBorders tall)
                ||| noBorders monocle
            )
        $ onWorkspace (S.getWs 7 c) floats
        $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
        getPerScreenLayout tall
            ||| getPerScreenLayout magnify
            ||| noBorders monocle
            ||| getPerScreenLayout floats
            ||| noBorders (tabs c)
