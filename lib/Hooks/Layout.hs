module Hooks.Layout
  ( myLayoutHook
  ) where

import           XMonad                         ( (|||) )
import           XMonad.Actions.MouseResize     ( mouseResize )
import           XMonad.Hooks.ManageDocks       ( avoidStruts )
import           XMonad.Layout.MultiToggle      ( (??)
                                                , EOT(EOT)
                                                , mkToggle
                                                )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers
                                                  ( NBFULL
                                                  , NOBORDERS
                                                  )
                                                )
import           XMonad.Layout.NoBorders        ( noBorders )
import           XMonad.Layout.PerWorkspace     ( onWorkspace
                                                , onWorkspaces
                                                )
import           XMonad.Layout.ToggleLayouts    ( toggleLayouts )
import           XMonad.Layout.WindowArranger   ( windowArrange )

import           Layout                         ( floats
                                                , magnify
                                                , monocle
                                                , tabs
                                                , tall
                                                )
import           Layout.PerScreen               ( getPerScreenLayout )
import           Settings                       ( myWorkspaces )

myLayoutHook =
  avoidStruts
    $ mouseResize
    $ windowArrange
    $ toggleLayouts floats
    $ onWorkspaces [head myWorkspaces, myWorkspaces !! 4]
                   (noBorders monocle ||| getPerScreenLayout tall)
    $ onWorkspace
        (myWorkspaces !! 1)
        (   getPerScreenLayout (noBorders magnify)
        ||| getPerScreenLayout (noBorders tall)
        ||| noBorders monocle
        )
    $ onWorkspace (myWorkspaces !! 7) floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
 where
  myDefaultLayout =
    getPerScreenLayout tall
      ||| getPerScreenLayout magnify
      ||| noBorders monocle
      ||| getPerScreenLayout floats
      ||| noBorders tabs
