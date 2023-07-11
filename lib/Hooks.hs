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
                                                )
import           XMonad.Hooks.ManageDocks       ( checkDock )
import           XMonad.Hooks.ManageHelpers     ( doLower )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Util.SpawnOnce          ( spawnOnce )

import           Data.Monoid                    ( Endo )

import           Settings                       ( myWorkspaces )

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "start_background"
  spawnOnce "picom &"
  spawn "eww_ctrl open"
  setWMName "LG3Dw"

myManagementHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManagementHook = composeAll
  [ className =? "htop" --> doShift (myWorkspaces !! 3)
  , className =? "waterfox" --> doShift (myWorkspaces !! 1)
  , className =? "code" --> doShift (head myWorkspaces)
  , className =? "Thunderbird" --> doShift (myWorkspaces !! 5)
  , className =? "Spotify" --> doShift (myWorkspaces !! 6)
  , className =? "discord" --> doShift (myWorkspaces !! 4)
  , className =? "Signal" --> doShift (myWorkspaces !! 4)
  , className =? "Gimp" --> doShift (myWorkspaces !! 7)
  , className =? "Gimp" --> doFloat
  , (className =? "waterfox" <&&> resource =? "Dialog") --> doFloat
  , checkDock --> doLower
  ]
