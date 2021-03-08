module Keys
  ( myKeys
  ) where

import           System.Exit                    ( exitSuccess )
import           XMonad                         ( ChangeLayout(NextLayout)
                                                , Resize(Expand, Shrink)
                                                , X
                                                , io
                                                , sendMessage
                                                , spawn
                                                , windows
                                                , withFocused
                                                )
import           XMonad.Actions.CopyWindow      ( kill1 )
import           XMonad.Actions.CycleWS         ( nextScreen
                                                , nextWS
                                                , prevScreen
                                                , prevWS
                                                )
import           XMonad.Actions.Promote         ( promote )
import           XMonad.Actions.RotSlaves       ( rotAllDown
                                                , rotSlavesDown
                                                )
import           XMonad.Actions.WithAll         ( killAll
                                                , sinkAll
                                                )
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..) )
import           XMonad.Layout.ResizableTile    ( MirrorResize(..) )
import qualified XMonad.Layout.ToggleLayouts   as T
                                                ( ToggleLayout(Toggle) )
import           XMonad.Layout.WindowArranger   ( WindowArrangerMsg(..) )
import qualified XMonad.StackSet               as W

import           Layouts                        ( toggleFullscreen )
import           Settings                       ( myTerminal )

myNamedKeys :: [(String, String, X ())]
myNamedKeys =
  [ ("Recompile XMonad", "M-C-r", spawn "xmonad --recompile")
  , ("Restart XMonad", "M-S-r", spawn "xmonad --restart")
  , ("Quit XMonad", "M-S-q", io exitSuccess)

    -- Open my preferred terminal
  , ("Open Terminal", "M-<Return>", spawn myTerminal)

    -- Rofi
  , ("Application Menu", "M-S-<Return>", spawn "rofi -show drun")
  , ("Run Command", "M-C-<Return>", spawn "rofi -show run")
  , ("Switch Window", "M-<Tab>", spawn "rofi -show window")
  , ("Power Menu", "C-M1-<Delete>", spawn "rofi -show power-menu")

    -- Windows
  , ("Close Current Window", "M-w", kill1)
  , ("Close All Windows On Workspaces", "M-S-w", killAll)

    -- Floating windows
  , ("Toggles 'floats' Layout", "M-f", sendMessage (T.Toggle "floats"))
  , ( "Push Floating Window To Tile"
    , "M-<Delete>"
    , withFocused $ windows . W.sink
    )
  , ("Push All Floating Windows To Tile", "M-S-<Delete>" , sinkAll)

    -- Windows navigation
  , ("Move To Master Window", "M-m", windows W.focusMaster)
  , ("Move To Next Window"              , "M-j"          , windows W.focusDown)
  , ("Move To Prev Window"              , "M-k"          , windows W.focusUp)
  , ("Swap Focused And Master Window"   , "M-S-m"        , windows W.swapMaster)
  , ("Swap Focused With Next Window"    , "M-S-j"        , windows W.swapDown)
  , ("Swap Focused With Prev Window"    , "M-S-k"        , windows W.swapUp)
  , ("Moves Focused Window To Master"   , "M-<Backspace>", promote)
  , ("Rotate All Windows Except Master" , "M1-S-<Tab>"   , rotSlavesDown)
  , ("Rotate All Windows"               , "M1-C-<Tab>"   , rotAllDown)

      -- Layouts
  , ("Switch To Next Layout", "M-S-<Tab>", sendMessage NextLayout)
  , ("Arrange"                          , "M-C-M1-<Up>"  , sendMessage Arrange)
  , ("DeArrange", "M-C-M1-<Down>", sendMessage DeArrange)
  , ("Toggle Fullscreen"                , "M-<Space>"    , toggleFullscreen)
  , ("Toggle Struts", "M-S-<Space>", sendMessage ToggleStruts)

    -- Window manipulation
  , ("Shrink Window Horiz"              , "M-h"          , sendMessage Shrink)
  , ("Expand Window Horiz"              , "M-l"          , sendMessage Expand)
  , ("Shrink Window Vert", "M-C-j", sendMessage MirrorShrink)
  , ("Expand Window Vert", "M-C-k", sendMessage MirrorExpand)

    -- Tools
  , ("Screenshot", "M-S-s", spawn "flameshot gui")

    -- Monitors
  , ("Switch To Next Monitor"           , "M-."          , nextScreen)
  , ("Switch To Prev Monitor"           , "M-,"          , prevScreen)

    -- Workspaces
  , ("Switch To Next Monitor"           , "M-S-."        , nextWS)
  , ("Switch To Prev Monitor"           , "M-S-,"        , prevWS)
  ]

extractKeys :: (String, String, X ()) -> (String, X ())
extractKeys (_, key, func) = (key, func)

myKeys :: [(String, X ())]
myKeys = map extractKeys myNamedKeys
