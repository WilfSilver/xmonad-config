module Hooks
  ( myStartupHook
  , myManagementHook
  ) where

import           XMonad                         ( (-->)
                                                , (<&&>)
                                                , (=?)
                                                , Atom
                                                , Query
                                                , Window
                                                , WindowSet
                                                , X
                                                , ask
                                                , className
                                                , composeAll
                                                , doFloat
                                                , doShift
                                                , getAtom
                                                , liftX
                                                , resource
                                                , spawn
                                                )
import           XMonad.Hooks.ManageDocks       ( checkDock )
import           XMonad.Hooks.ManageHelpers     ( doLower )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Util.WindowProperties   ( getProp32 )

import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Endo )

import           Settings                       ( myWorkspaces )

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawn "~/.config/eww/launch_bar"
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
