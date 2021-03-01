module Settings
  ( myFont
  , myModMask
  , myTerminal
  , myBorderWidth
  , myNormColour
  , myFocusColour
  , myWorkspaces
  ) where

import           XMonad                         ( Dimension
                                                , KeyMask
                                                , mod4Mask
                                                )

myFont :: String
myFont = "xft:FiraCode:bold:size=20:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets mod key to super/windows key

myTerminal :: String
myTerminal = "terminator"   -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColour :: String
myNormColour = "#282c34"  -- Border color of normal windows

myFocusColour :: String
myFocusColour = "#bbc5ff"  -- Border color of focused windows

myWorkspaces :: [String]
myWorkspaces =
    -- dev      www       sys       doc      chat        mus      gfx
  ["\xf121", "\xe007", "\xf120", "\xf02d", "\xf392", "\xf001", "\xf0c2"]
    -- 1         2         3         4         5         6         7
