module Hooks (
    myStartupHook,
    myManagementHook,
) where

import XMonad (
    Query,
    WindowSet,
    X,
    className,
    composeAll,
    doFloat,
    doShift,
    resource,
    spawn,
    (-->),
    (<&&>),
    (=?),
 )
import XMonad.Hooks.ManageDocks (checkDock)
import XMonad.Hooks.ManageHelpers (doLower)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.SpawnOnce (spawnOnce)

import Data.Monoid (Endo)

import Data.Maybe (fromMaybe)
import qualified Settings as S

runStartup :: [S.StartUp] -> X ()
runStartup [] = return ()
runStartup (x : xs) = do
    if fromMaybe False (S.once x) then spawnOnce process else spawn process
    runStartup xs
  where
    process = S.process x

myStartupHook :: S.Config -> X ()
myStartupHook c = do
    runStartup $ S.startup c
    setWMName "LG3Dw"

myManagementHook :: S.Config -> XMonad.Query (Data.Monoid.Endo WindowSet)
myManagementHook c =
    composeAll
        [ className =? "waterfox" --> doShift (S.getWs 1 c)
        , className =? "code" --> doShift (S.getWs 0 c)
        , className =? "Thunderbird" --> doShift (S.getWs 5 c)
        , className =? "Spotify" --> doShift (S.getWs 6 c)
        , className =? "discord" --> doShift (S.getWs 4 c)
        , className =? "Signal" --> doShift (S.getWs 4 c)
        , className =? "Gimp" --> doShift (S.getWs 7 c)
        , className =? "Gimp" --> doFloat
        , (className =? "waterfox" <&&> resource =? "Dialog") --> doFloat
        , checkDock --> doLower
        ]
