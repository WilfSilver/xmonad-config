module Hooks
    ( eventLogHookForPolyBar
    , myStartupHook
    , myManagementHook
    ) where

import           Control.Monad                  ( join )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Endo )

import           XMonad                         ( (-->)
                                                , (<&&>)
                                                , (=?)
                                                , Query
                                                , WindowSet
                                                , X
                                                , XState(windowset)
                                                , className
                                                , composeAll
                                                , doFloat
                                                , doShift
                                                , gets
                                                , io
                                                , resource
                                                , title
                                                )
import           XMonad.Hooks.SetWMName         ( setWMName )
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Util.SpawnOnce          ( spawnOnce )


import           Settings                       ( myWorkspaces )

shortenTitle :: String -> String
shortenTitle x = if length x > 50 then take 47 x ++ "..." else x

eventLogHookForPolyBar :: X ()
eventLogHookForPolyBar = do
    winset   <- gets windowset
    winTitle <- maybe (return "") (fmap show . getName) . W.peek $ winset
    let currWs = W.currentTag winset
    let wsStr  = join $ map (fmt currWs) myWorkspaces

    io $ writeFile "/tmp/.xmonad-title-log" $ shortenTitle winTitle
    io $ writeFile "/tmp/.xmonad-workspace-log" wsStr

  where
    fmt currWs ws
        | currWs == ws
        = "%{F#fc0511}" ++ ws ++ "%{F-}  "
        | otherwise
        = "%{A1:xdotool key super+"
            ++ show (fromMaybe 0 (elemIndex ws myWorkspaces) + 1)
            ++ ":}"
            ++ ws
            ++ "%{A}  "

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom &"
    spawnOnce "conky &"
    spawnOnce "discord"
    spawnOnce "~/scripts/polybar/launch.sh"
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
