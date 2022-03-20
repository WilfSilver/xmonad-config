module Dock
    ( dockEventLogHook,
      dockStartupHook
    ) where

import           Control.Monad                  ( forM_
                                                , join
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromMaybe )
import           Settings                       ( myCurrentWsColour
                                                , myNonEmptyWsColour
                                                , myVisibleWsColour
                                                , myWorkspaces
                                                )

import           XMonad                         ( ScreenDetail
                                                , WorkspaceId
                                                , X
                                                , XState(windowset)
                                                , gets
                                                , io
                                                )
import           XMonad.Config.Prime            ( Window )
import           XMonad.Core                    ( Layout
                                                , ScreenId
                                                , description
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.Run                ( safeSpawn )

import           Util                           ( getNonEmptyWorkspaces
                                                , getWindowTitle
                                                , visibleWorkspaces
                                                , currentWorkspace
                                                )

-- This runs when xmonad starts up and will create the files that are needed for communication
dockStartupHook :: X ()
dockStartupHook = do
  winset <- gets windowset
  createDockFiles $ W.screens winset


-- Recursively goes through each screen creating its file set
createDockFiles
  :: [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail] -> X ()
createDockFiles []       = return ()
createDockFiles (x : xs) = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"]
    $ \file -> safeSpawn "touch" ["/tmp/" ++ file ++ screenID]
  createDockFiles xs
  where screenID = drop 2 $ show $ W.screen x

-- Event hook for the dock, updates the files to have correct information on them
dockEventLogHook :: X ()
dockEventLogHook = do
    winset <- gets windowset
    visWs  <- visibleWorkspaces
    currWs <- currentWorkspace
    let nonEmptWs = getNonEmptyWorkspaces (W.hidden winset)
    let wsStr = join
            $ map (formatWorkspaces currWs visWs nonEmptWs) myWorkspaces

    writeScreenLogFiles (W.screens winset) wsStr

-- Formats all the workspaces correctly, so that each icon is clickable, taking to the correct workspace and has the correct colour to signify if it is displayed or has windows on it
formatWorkspaces
    :: WorkspaceId -> [WorkspaceId] -> [WorkspaceId] -> WorkspaceId -> String
formatWorkspaces currWs visWs nonEmptWs ws
    | ws == currWs
    = "%{F" ++ myCurrentWsColour ++ "}" ++ ws ++ "%{F-}  "
    | ws `elem` visWs
    = "%{F"
        ++ myVisibleWsColour
        ++ "}"
        ++ getActionKey ws
        ++ ws
        ++ "%{A}%{F-}  "
    | ws `elem` nonEmptWs
    = "%{F"
        ++ myNonEmptyWsColour
        ++ "}"
        ++ getActionKey ws
        ++ ws
        ++ "%{A}%{F-}  "
    | otherwise
    = getActionKey ws ++ ws ++ "%{A}  "

-- Creates the correct command for going to a given workspace
getActionKey :: WorkspaceId -> String
getActionKey ws =
    "%{A1:xdotool key super+"
        ++ show (fromMaybe 0 (elemIndex ws myWorkspaces) + 1)
        ++ ":}"

-- Writes all log files for each screen individually (uses recursion)
writeScreenLogFiles
    :: [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
    -> String
    -> X ()
writeScreenLogFiles []       _     = return ()
writeScreenLogFiles (x : xs) fmtWs = do
    winTitle <- maybe (return " ") (getWindowTitle . W.focus) maybeStack

    -- Writes to the title and workspace log files with the screen id on the end e.g. /tmp/.xmonad-title-log0
    io $ writeFile (titleLogFile ++ screenID) $ shortenTitle winTitle
    io $ writeFile (workspaceLogFile ++ screenID) $ addLayoutName
        (W.layout workspace)
        fmtWs

    -- Recursively goes through the screens
    writeScreenLogFiles xs fmtWs
  where
    -- stores the stack in a maybe format
    maybeStack       = W.stack workspace
    -- Gets the screen ID from the screen and drops the first two characters so instead for it being "S 0" it is "0"
    screenID         = drop 2 $ show $ W.screen x
    -- Gets the current workspace and screen
    workspace        = W.workspace x
    -- Log file locations
    titleLogFile     = "/tmp/.xmonad-title-log"
    workspaceLogFile = "/tmp/.xmonad-workspace-log"

-- Adds the given layout's name to the left of the workspace's icons
addLayoutName :: Layout Window -> String -> String
addLayoutName layout contents =
    "%{A1:xdotool key super+shift+Tab:}"
        ++ description layout
        ++ "%{A} | "
        ++ contents

-- Shortens the title if it is too long
shortenTitle :: String -> String
shortenTitle x = if length x > 50 then take 47 x ++ "..." else x
