{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooks.Dock (
    dockEventLogHook,
    dockStartupHook,
) where

import Control.Monad (forM_)
import Data.Aeson (
    FromJSON,
    ToJSON,
    encode,
 )
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (sortOn)
import GHC.Generics (Generic)

import XMonad (
    ScreenDetail,
    Window,
    WorkspaceId,
    X,
    XState (windowset),
    gets,
    io,
 )
import XMonad.Core (
    Layout,
    ScreenId,
    description,
 )
import qualified XMonad.StackSet as W
import XMonad.Util.Run (safeSpawn)

import Commands (
    getWsShiftCmd,
    getWsViewCmd,
 )

import qualified Settings as S
import Util (
    currentWorkspace,
    getNonEmptyWorkspaces,
    getWindowTitle,
    visibleWorkspaces,
 )

-- Generalised information that is specific to each workspace
data WorkspaceInfo = WorkspaceInfo
    { name :: !String
    , state :: !String
    , action :: !String
    , sAction :: !String
    }
    deriving (Generic, Show)

instance ToJSON WorkspaceInfo
instance FromJSON WorkspaceInfo

-- Information that is specific to each screen
data PerScreenInfo = PerScreenInfo
    { title :: !String
    , layout :: !String
    }
    deriving (Generic, Show)

instance ToJSON PerScreenInfo
instance FromJSON PerScreenInfo

-- This runs when xmonad starts up and will create the files that are needed for communication
dockStartupHook :: X ()
dockStartupHook = do
    winset <- gets windowset
    createDockFiles $ W.screens winset

-- Recursively goes through each screen creating its file set
createDockFiles ::
    [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail] -> X ()
createDockFiles [] = return ()
createDockFiles (x : xs) = do
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $
        \file -> safeSpawn "touch" ["/tmp/" ++ file ++ screenID]
    createDockFiles xs
  where
    screenID = drop 2 $ show $ W.screen x

-- Event hook for the dock, updates the files to have correct information on them
dockEventLogHook :: S.Config -> X ()
dockEventLogHook c = do
    winset <- gets windowset
    visWs <- visibleWorkspaces
    currWs <- currentWorkspace
    let nonEmptWs = getNonEmptyWorkspaces (W.hidden winset)
    let worksInfo = map (getWorkspaceInfo currWs visWs nonEmptWs) (S.workspaces c)

    perScreenInfo <- getAllPerScreenInfo $ sortOn W.screen (W.screens winset)

    io $ writeFile titleLogFile $ (toString . encode) perScreenInfo
    io $ writeFile workspaceLogFile $ (toString . encode) worksInfo
  where
    -- Log file locations TODO: Move to settings
    titleLogFile = "/tmp/.xmonad-per-screen-log"
    workspaceLogFile = "/tmp/.xmonad-workspace-log"

getWsState ::
    WorkspaceId -> [WorkspaceId] -> [WorkspaceId] -> WorkspaceId -> String
getWsState currWs visWs nonEmptWs ws
    | ws == currWs = "active"
    | ws `elem` visWs = "visible"
    | ws `elem` nonEmptWs = "occupied"
    | otherwise = "empty"

getWorkspaceInfo ::
    WorkspaceId ->
    [WorkspaceId] ->
    [WorkspaceId] ->
    WorkspaceId ->
    WorkspaceInfo
getWorkspaceInfo currWs visWs nonEmptWs ws =
    WorkspaceInfo
        { name = ws
        , state = myState
        , action = myAction
        , sAction = mySAction
        }
  where
    myState = getWsState currWs visWs nonEmptWs ws
    myAction = (returnIfActive . getActionKey) ws myState
    mySAction = (returnIfActive . getSecondaryActionKey) ws myState

returnIfActive :: String -> String -> String
returnIfActive r myState
    | myState == "active" = ""
    | otherwise = r

-- Creates the correct command for going to a given workspace
getActionKey :: WorkspaceId -> String
getActionKey ws = "xmonadctl " ++ getWsViewCmd ws

getSecondaryActionKey :: WorkspaceId -> String
getSecondaryActionKey ws = "xmonadctl " ++ getWsShiftCmd ws

getAllPerScreenInfo ::
    [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail] ->
    X [PerScreenInfo]
getAllPerScreenInfo [] = return []
getAllPerScreenInfo (x : xs) = do
    info <- getPerScreenInfo x
    others <- getAllPerScreenInfo xs
    return $ info : others

getPerScreenInfo ::
    W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail ->
    X PerScreenInfo
getPerScreenInfo screen = do
    winTitle <- getTitleFor workspace
    return PerScreenInfo{title = winTitle, layout = description myLayout}
  where
    myLayout = W.layout workspace
    workspace = W.workspace screen

-- getTitleFor :: W.Workspace (Layout Window) -> X String
getTitleFor ws = do
    winTitle <- maybe (return " ") (getWindowTitle . W.focus) maybeStack
    return $ shortenTitle winTitle
  where
    -- stores the stack in a maybe format
    maybeStack = W.stack ws

shortenTitle :: String -> String
shortenTitle x
    | length x > 50 = take 47 x ++ "..."
    | otherwise = x
