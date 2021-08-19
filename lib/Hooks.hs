module Hooks
  ( myStartupHook
  , myManagementHook
  ) where

import           XMonad                         ( (-->)
                                                , (<&&>)
                                                , (=?)
                                                , Query
                                                , WindowSet
                                                , X
                                                , className
                                                , composeAll
                                                , doFloat
                                                , doShift
                                                , resource
                                                , spawn
                                                , title
                                                )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Util.SpawnOnce          ( spawnOnce )

import           Data.Monoid                    ( Endo )

import           Settings                       ( myWorkspaces )

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawn "~/.config/polybar/bin/launch.sh"
  setWMName "LG3Dw"

myManagementHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManagementHook = composeAll
  [ className =? "htop" --> doShift (myWorkspaces !! 3)
  , className =? "waterfox-g3" --> doShift (myWorkspaces !! 1)
  , className =? "code" --> doShift (head myWorkspaces)
  , className =? "Thunderbird" --> doShift (myWorkspaces !! 5)
  , className =? "Spotify" --> doShift (myWorkspaces !! 6)
  , title =? "Discord" --> doShift (myWorkspaces !! 4)
  , title =? "Signal" --> doShift (myWorkspaces !! 4)
  , title =? "Teams" --> doShift (myWorkspaces !! 4)
  , className =? "Gimp" --> doShift (myWorkspaces !! 7)
  , className =? "Gimp" --> doFloat
  , (className =? "waterfox-g3" <&&> resource =? "Dialog") --> doFloat
  ]
