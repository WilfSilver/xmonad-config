module Dock
  ( dockEventLogHook
  , dockStartupHook
  ) where

import           Control.Monad                  ( forM_ )
import           Data.List                      ( elemIndex )
import           Data.List.Utils                ( join )
import           Data.Maybe                     ( fromMaybe )

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
                                                -- , description
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.Run                ( safeSpawn )

import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.List                      ( sortOn )
import           Settings                       ( myWorkspaces )
import           Util                           ( currentWorkspace
                                                , getNonEmptyWorkspaces
                                                , getWindowTitle
                                                , visibleWorkspaces
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
  let wsStr =
        join "\n" $ map (formatWorkspaces currWs visWs nonEmptWs) myWorkspaces
  titles <- getAllTitlesFor $ sortOn W.screen (W.screens winset)

  io $ writeFile titleLogFile $ (unpack . encode) titles
  io $ writeFile workspaceLogFile $ wsStr
 where
  -- Log file locations TODO: Move to settings
  titleLogFile     = "/tmp/.xmonad-title-log"
  workspaceLogFile = "/tmp/.xmonad-workspace-log"

-- Formats all the workspaces correctly, so that each icon is clickable, taking to the correct workspace and has the correct colour to signify if it is displayed or has windows on it
formatWorkspaces
  :: WorkspaceId -> [WorkspaceId] -> [WorkspaceId] -> WorkspaceId -> String
formatWorkspaces currWs visWs nonEmptWs ws
  | ws == currWs        = ws ++ ":active:"
  | ws `elem` visWs     = ws ++ ":visible:" ++ getActionKey ws
  | ws `elem` nonEmptWs = ws ++ ":occupied:" ++ getActionKey ws
  | otherwise           = ws ++ ":empty:" ++ getActionKey ws

-- Creates the correct command for going to a given workspace
getActionKey :: WorkspaceId -> String
getActionKey ws =
  "xdotool key super+" ++ show (fromMaybe 0 (elemIndex ws myWorkspaces) + 1)

getAllTitlesFor
  :: [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
  -> X [String]
getAllTitlesFor []       = return []
getAllTitlesFor (x : xs) = do
  winTitle      <- maybe (return " ") (getWindowTitle . W.focus) maybeStack
  other_strings <- getAllTitlesFor xs
  return $ winTitle : other_strings
 where
  -- stores the stack in a maybe format
  maybeStack = W.stack workspace
  -- Gets the current workspace and screen
  workspace  = W.workspace x
