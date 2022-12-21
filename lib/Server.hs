module Server
  ( myCommands
  , runCommand
  , myServerEventHook
  ) where

-- Extension to XMonad.Actions.Commands to have custom commands

import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           System.Exit                    ( exitSuccess )
import           XMonad                         ( (<+>)
                                                , ChangeLayout(NextLayout)
                                                , Resize(Expand, Shrink)
                                                , X
                                                , XConfig(layoutHook, terminal)
                                                , asks
                                                , config
                                                , io
                                                , kill
                                                , refresh
                                                , restart
                                                , sendMessage
                                                , setLayout
                                                , spawn
                                                , windows
                                                , withFocused
                                                )
import           XMonad.Actions.Commands        ( commandMap
                                                , screenCommands
                                                , workspaceCommands
                                                )
import           XMonad.Hooks.ServerMode        ( serverModeEventHook'
                                                , serverModeEventHookCmd'
                                                , serverModeEventHookF
                                                )
import           XMonad.StackSet                ( focusDown
                                                , focusUp
                                                , sink
                                                , swapDown
                                                , swapMaster
                                                , swapUp
                                                )

myCommands :: X [(String, X ())]
myCommands = do
  wscmds <- workspaceCommands
  return $ wscmds ++ screenCommands ++ otherCommands
 where
  otherCommands =
    [ ("shrink"              , sendMessage Shrink)
    , ("expand"              , sendMessage Expand)
    , ("next-layout"         , sendMessage NextLayout)
    , ("default-layout", asks (layoutHook . config) >>= setLayout)
    , ("restart-wm"          , restart "xmonad" True)
    , ("restart-wm-no-resume", restart "xmonad" False)
    , ("xterm"               , spawn =<< asks (terminal . config))
    , ("run", spawn "exe=`dmenu_path | dmenu -b` && exec $exe")
    , ("kill"                , kill)
    , ("refresh"             , refresh)
    , ("focus-up"            , windows focusUp)
    , ("focus-down"          , windows focusDown)
    , ("swap-up"             , windows swapUp)
    , ("swap-down"           , windows swapDown)
    , ("swap-master"         , windows swapMaster)
    , ("sink"                , withFocused $ windows . sink)
    , ("quit-wm"             , io $ exitSuccess)
    ]

-- | Given the name of a command from 'myCommands', return the
--   corresponding action (or the null action if the command is not
--   found).
runCommand :: String -> X ()
runCommand c = do
  m <- fmap commandMap myCommands
  fromMaybe (return ()) (M.lookup c m)

myServerEventHook =
  serverModeEventHookCmd' myCommands
    <+> serverModeEventHook' myCommands
    <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
