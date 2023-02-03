module Settings
  ( myFont
  , myModMask
  , myTerminal
  , myBorderWidth
  , myNormColour
  , myFocusColour
  , myCurrentWsColour
  , myVisibleWsColour
  , myNonEmptyWsColour
  , myLayoutBackgroundColour
  , myWorkspaceBackgroundColour
  , myBarBackgroundColour
  , myWorkspaces
  ) where

import           XMonad                         ( Dimension
                                                , KeyMask
                                                , WorkspaceId
                                                , mod4Mask
                                                )

myFont :: String
myFont = "xft:FiraCode:bold:size=11:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets mod key to super/windows key

myTerminal :: String
myTerminal = "alacritty"   -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColour :: String
myNormColour = "#282c34"  -- Border color of normal windows

myFocusColour :: String
myFocusColour = "#bbc5ff"  -- Border color of focused windows

myCurrentWsColour :: String
myCurrentWsColour = "#fc0511" -- Colour of the current active workspace in the bar

myVisibleWsColour :: String
myVisibleWsColour = "#e88700" -- Colour of the visible workspaces in the bar (e.g. workspaces on different screens)

myNonEmptyWsColour :: String
myNonEmptyWsColour = "#e8e000" -- Colour of the workspaces with windows open on them

myLayoutBackgroundColour :: String
myLayoutBackgroundColour = "#282c34"

myWorkspaceBackgroundColour :: String
myWorkspaceBackgroundColour = "#383d47"

myBarBackgroundColour :: String
myBarBackgroundColour = "#282c34"

myWorkspaces :: [WorkspaceId]
myWorkspaces =
  [ "\xf121" -- dev   1
  , "\xf7a2" -- www   2
  , "\xf120" -- sys   3
  , "\xf02d" -- doc   4
  , "\xf392" -- chat  5
  , "\xf0e0" -- mail  6
  , "\xf001" -- music 7
  , "\xf0c2" -- gfx   8
  ]
