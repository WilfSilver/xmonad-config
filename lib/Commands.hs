module Commands
  ( defaultCommands
  , getCommand
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
  [ ("xmonadRecompile"   , spawn "xmonad --recompile")
  , ("xmonadRestart"     , restart "xmonad" True)
  , ("xmonadQuit"        , io exitSuccess)
  , ("closeCurrent"      , kill1)
  , ("closeAllWs"        , killAll)
  , ("toggleFloat"       , sendMessage (T.Toggle "floats"))
  , ("floatToTile"       , withFocused $ windows . W.sink)
  , ("allFloatToTile"    , sinkAll)
  , ("moveToMasterWin"   , windows W.focusMaster)
  , ("moveToNextWin"     , windows W.focusDown)
  , ("moveToPrevWin"     , windows W.focusUp)
  , ("swapFocusMasterWin", windows W.swapMaster)
  , ("swapFocusNextWin"  , windows W.swapDown)
  , ("swapFocusPrevWin"  , windows W.swapUp)
  , ("moveFocusMasterWin", promote)
  , ("rotateAllExMaster" , rotSlavesDown)
  , ("toggleFloat"       , rotAllDown)
  , ("switchNextLayout"  , sendMessage NextLayout)
  , ("arrange"           , sendMessage Arrange)
  , ("deArrange"         , sendMessage DeArrange)
  , ("fullscreen"        , toggleFullscreen)
  , ("strutsToggle"      , sendMessage ToggleStruts)
  , ("shrinkWinHorz"     , sendMessage Shrink)
  , ("expWinHorz"        , sendMessage Expand)
  , ("shrinkWinVert"     , sendMessage MirrorShrink)
  , ("expWinVert"        , sendMessage MirrorExpand)
  , ("switchNextMonitor" , nextScreen)
  , ("switchPrevMonitor" , prevScreen)
  , ("switchNextWs"      , nextWS)
  , ("switchPrevWs"      , prevWS)
  , ("term"              , spawn myTerminal)
  , ("termScratch"       , spawnScratchPad "term")
  , ("volumeScratch"     , spawnScratchPad "pavucontrol")
  ]

-- Altered from XMonad.Actions.Commands
getCommand :: Text -> X ()
getCommand cmd = do
  fromMaybe (return ()) (lookup cmd myTextCommands)

myCommands :: [(String, X ())]
myCommands = defaultCommands ++ workspaceCommands

myTextCommands :: [(Text, X ())]
myTextCommands = map convertToText myCommands

workspaceCommands :: [(String, X ())]
workspaceCommands = (workspaceCommands' . reverse) myWorkspaces

-- Internal

getCommandsForWs :: WorkspaceId -> Int -> [(String, X ())]
getCommandsForWs wsid i =
  [ ("shift_" ++ wsid  , windows $ W.shift wsid)
  , ("view_" ++ wsid   , windows $ W.view wsid)
  , ("shift_" ++ show i, windows $ W.shift wsid)
  , ("view_" ++ show i , windows $ W.view wsid)
  ]

workspaceCommands' :: [WorkspaceId] -> [(String, X ())]
workspaceCommands' []       = []
workspaceCommands' (x : []) = getCommandsForWs x 1
workspaceCommands' (x : xs) =
  (getCommandsForWs x $ 1 + (length xs)) ++ workspaceCommands' xs

convertToText :: (String, X ()) -> (Text, X ())
convertToText (x, v) = (pack x, v)
