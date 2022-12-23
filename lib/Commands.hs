module Commands
  ( defaultCommands
  , getCommand
  , getWsShiftCmd
  , getWsViewCmd
  , myCommands
  , myTextCommands
  , workspaceCommands
  ) where

import           System.Exit                    ( exitSuccess )
import           XMonad                         ( ChangeLayout(NextLayout)
                                                , Resize(Expand, Shrink)
                                                , WorkspaceId
                                                , X
                                                , io
                                                , restart
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

import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , pack
                                                )

import           Layouts                        ( toggleFullscreen )
import           ScratchPad                     ( spawnScratchPad )
import           Settings                       ( myTerminal
                                                , myWorkspaces
                                                )

defaultCommands :: [(String, X ())]
defaultCommands =
  [ ("recompile"        , spawn "xmonad --recompile")
  , ("restart"          , restart "xmonad" True)
  , ("quit"             , io exitSuccess)
  , ("close"            , kill1)
  , ("close-all"        , killAll)
  , ("toggle-float"     , sendMessage (T.Toggle "floats"))
  , ("float-to-tile"    , withFocused $ windows . W.sink)
  , ("all-float-to-tile", sinkAll)
  , ("focus-master"     , windows W.focusMaster)
  , ("focus-next"       , windows W.focusDown)
  , ("focus-prev"       , windows W.focusUp)
  , ("swap-with-master" , windows W.swapMaster)
  , ("swap-with-next"   , windows W.swapDown)
  , ("swap-with-prev"   , windows W.swapUp)
  , ("promote"          , promote)
  , ("rotate-slaves"    , rotSlavesDown)
  , ("rotate-all"       , rotAllDown)
  , ("next-layout"      , sendMessage NextLayout)
  , ("arrange"          , sendMessage Arrange)
  , ("dearrange"        , sendMessage DeArrange)
  , ("fullscreen"       , toggleFullscreen)
  , ("toggle-struts"    , sendMessage ToggleStruts)
  , ("shrink-horz"      , sendMessage Shrink)
  , ("expand-horz"      , sendMessage Expand)
  , ("shrink-vert"      , sendMessage MirrorShrink)
  , ("expand-vert"      , sendMessage MirrorExpand)
  , ("next-screen"      , nextScreen)
  , ("prev-screen"      , prevScreen)
  , ("next-ws"          , nextWS)
  , ("prev-ws"          , prevWS)
  , ("term"             , spawn myTerminal)
  , ("scratch-term"     , spawnScratchPad "term")
  , ("scratch-volume"   , spawnScratchPad "pavucontrol")
  ]

getCommand :: Text -> X ()
getCommand cmd = do
  fromMaybe (return ()) (lookup cmd myTextCommands)

getWsShiftCmd :: String -> String
getWsShiftCmd ws = "shift-" ++ ws

getWsViewCmd :: String -> String
getWsViewCmd ws = "view-" ++ ws

myCommands :: [(String, X ())]
myCommands = defaultCommands ++ workspaceCommands

myTextCommands :: [(Text, X ())]
myTextCommands = map convertToText myCommands

workspaceCommands :: [(String, X ())]
workspaceCommands = (workspaceCommands' . reverse) myWorkspaces

-- Internal

getCommandsForWs :: WorkspaceId -> Int -> [(String, X ())]
getCommandsForWs wsid i =
  [ (getWsShiftCmd wsid    , windows $ W.shift wsid)
  , (getWsViewCmd wsid     , windows $ W.view wsid)
  , (getWsShiftCmd $ show i, windows $ W.shift wsid)
  , (getWsViewCmd $ show i , windows $ W.view wsid)
  ]

workspaceCommands' :: [WorkspaceId] -> [(String, X ())]
workspaceCommands' []       = []
workspaceCommands' (x : []) = getCommandsForWs x 1
workspaceCommands' (x : xs) =
  (getCommandsForWs x $ 1 + (length xs)) ++ workspaceCommands' xs

convertToText :: (String, X ()) -> (Text, X ())
convertToText (x, v) = (pack x, v)
