-- My XMonad configuration

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
import           XMonad.Hooks.ManageDocks       ( docks )
import           XMonad.Hooks.ServerMode        ( serverModeEventHook
                                                , serverModeEventHookCmd
                                                , serverModeEventHookF
                                                )
import           XMonad.Layout.Fullscreen       ( fullscreenEventHook
                                                , fullscreenManageHook
                                                , fullscreenSupport
                                                )
import           XMonad.Util.EZConfig           ( additionalKeysP )

import           Hooks                          ( myManagementHook
                                                , myStartupHook
                                                )
import           Keys                           ( myKeys )
import           Layouts                        ( myLayoutHook )
import           Dock                        ( dockEventLogHook
                                                , dockStartupHook
                                                )
import           Settings                       ( myBorderWidth
                                                , myFocusColour
                                                , myModMask
                                                , myNormColour
                                                , myTerminal
                                                , myWorkspaces
                                                )

main :: IO ()
main = do
  xmonad
    $ fullscreenSupport
    $ docks
    $ ewmh
    $ def
        { manageHook = fullscreenManageHook <+> myManagementHook
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT"
                                                        (io . putStrLn)
                               <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook <+> dockStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColour
        , focusedBorderColor = myFocusColour
        , logHook            = dockEventLogHook
        }
    `additionalKeysP` myKeys
