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
  spawnOnce "conky &"
  spawnOnce "discord"
  spawn "~/.config/polybar/scripts/launch.sh"
  spawnOnce "flameshot"
  setWMName "LG3Dw"

myManagementHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManagementHook = composeAll
  [ className =? "htop" --> doShift (myWorkspaces !! 3)
  , className =? "firefox" --> doShift (myWorkspaces !! 1)
  , className =? "code" --> doShift (head myWorkspaces)
  , title =? "Discord" --> doShift (myWorkspaces !! 4)
  , title =? "Teams" --> doShift (myWorkspaces !! 4)
  , className =? "Gimp" --> doShift (myWorkspaces !! 6)
  , className =? "Gimp" --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  ]
