module Polybar
    ( barEventLogHook
    ) where

import           Control.Monad                  ( join )
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
import           XMonad.Util.NamedWindows       ( getName )

barEventLogHook :: X ()
barEventLogHook = do
    winset <- gets windowset
    visWs  <- visibleWorkspaces
    let nonEmptWs = getNonEmptyWorkspaces (W.hidden winset)
    let currWs    = W.currentTag winset
    let wsStr = join
            $ map (formatWorkspaces currWs visWs nonEmptWs) myWorkspaces

    writeScreenLogFiles (W.screens winset) wsStr

visibleWorkspaces :: X [WorkspaceId]
visibleWorkspaces = do
    winset <- gets windowset
    return $ map (W.tag . W.workspace) (W.visible winset)

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

getNonEmptyWorkspaces :: [W.Workspace WorkspaceId l a] -> [WorkspaceId]
getNonEmptyWorkspaces []       = []
getNonEmptyWorkspaces (x : xs) = do
    let stack = W.stack x
    case stack of
        Nothing -> getNonEmptyWorkspaces xs
        Just _  -> W.tag x : getNonEmptyWorkspaces xs


getActionKey :: WorkspaceId -> String
getActionKey ws =
    "%{A1:xdotool key super+"
        ++ show (fromMaybe 0 (elemIndex ws myWorkspaces) + 1)
        ++ ":}"

writeScreenLogFiles
    :: [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
    -> String
    -> X ()
writeScreenLogFiles []       _     = return ()
writeScreenLogFiles (x : xs) fmtWs = do
    winTitle <- maybe (return "") (getWindowTitle . W.focus) maybeStack

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

addLayoutName :: Layout Window -> String -> String
addLayoutName layout contents =
    "%{A1:xdotool key super+shift+Tab:}"
        ++ description layout
        ++ "%{A} | "
        ++ contents

getWindowTitle :: Window -> X String
getWindowTitle win = do
    namedWin <- getName win
    return $ show namedWin


shortenTitle :: String -> String
shortenTitle x = if length x > 50 then take 47 x ++ "..." else x

