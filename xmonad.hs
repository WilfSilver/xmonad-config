---------------------------------
---          IMPORTS          ---
---------------------------------

import           Control.Monad                  ( forM_ )
  -- Base
import           XMonad                         ( (<+>)
                                                , Default(def)
                                                , XConfig
                                                  ( borderWidth
                                                  , focusedBorderColor
                                                  , handleEventHook
                                                  , layoutHook
                                                  , logHook
                                                  , manageHook
                                                  , modMask
                                                  , normalBorderColor
                                                  , startupHook
                                                  , terminal
                                                  , workspaces
                                                  )
                                                , io
                                                , xmonad
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks       ( docksEventHook
                                                , manageDocks
                                                )
import           XMonad.Hooks.ServerMode        ( serverModeEventHook
                                                , serverModeEventHookCmd
                                                , serverModeEventHookF
                                                )
import           XMonad.Layout.Fullscreen       ( fullscreenEventHook
                                                , fullscreenManageHook
                                                , fullscreenSupport
                                                )
import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.Run                ( safeSpawn )

import           Hooks                          ( myManagementHook
                                                , myStartupHook
                                                )
import           Keys                           ( myKeys )
import           Layouts                        ( myLayoutHook )
import           Polybar                        ( barEventLogHook )
import           Settings                       ( myBorderWidth
                                                , myFocusColour
                                                , myModMask
                                                , myNormColour
                                                , myTerminal
                                                , myWorkspaces
                                                )

main :: IO ()
main = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"]
    $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
  xmonad
    $                 fullscreenSupport
    $                 ewmh def
                        { manageHook = fullscreenManageHook <+> myManagementHook <+> manageDocks
                        , handleEventHook    = serverModeEventHookCmd
                                               <+> serverModeEventHook
                                               <+> serverModeEventHookF "XMONAD_PRINT"
                                                                        (io . putStrLn)
                                               <+> docksEventHook
                                               <+> fullscreenEventHook
                        , modMask            = myModMask
                        , terminal           = myTerminal
                        , startupHook        = myStartupHook
                        , layoutHook         = myLayoutHook
                        , workspaces         = myWorkspaces
                        , borderWidth        = myBorderWidth
                        , normalBorderColor  = myNormColour
                        , focusedBorderColor = myFocusColour
                        , logHook            = barEventLogHook
                        }
    `additionalKeysP` myKeys
