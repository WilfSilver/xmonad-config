-- My XMonad configuration

-- Base
import           XMonad                         ( (<+>)
                                                , Atom
                                                , Default(def)
                                                , X
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
                                                , asks
                                                , changeProperty32
                                                , getAtom
                                                , getWindowProperty32
                                                , liftIO
                                                , propModeAppend
                                                , theRoot
                                                , withDisplay
                                                , xmonad
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks       ( docks )
import           XMonad.Layout.Fullscreen       ( fullscreenEventHook
                                                , fullscreenManageHook
                                                , fullscreenSupport
                                                )
import           XMonad.Util.Cursor             ( setDefaultCursor
                                                , xC_left_ptr
                                                )
import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.NamedScratchpad    ( namedScratchpadManageHook )

import           Control.Monad                  ( when
                                                , join
                                                )
import           Data.Maybe                     ( maybeToList )

import           Hooks                          ( myManagementHook
                                                , myStartupHook
                                                )
import           Hooks.Dock                     ( dockEventLogHook
                                                , dockStartupHook
                                                )
import           Hooks.Server                   ( myServerEventHook )

import           Keys                           ( myKeys )
import           Layouts                        ( myLayoutHook )
import           ScratchPad                     ( myScratchPads )

import           Settings                       ( myBorderWidth
                                                , myFocusColour
                                                , myModMask
                                                , myNormColour
                                                , myTerminal
                                                , myWorkspaces
                                                )

-- Stolen from https://github.com/evanjs/gentoo-dotfiles/commit/cbf78364ea60e62466594340090d8e99200e8e08 (thanks)
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    root            <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    atom            <- getAtom "ATOM"
    liftIO $ do
       sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED root
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy root a_NET_SUPPORTED atom propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

main :: IO ()
main = do
  keys <- myKeys
  xmonad
    $ fullscreenSupport
    $ docks
    $ ewmh
    $ def
        { manageHook         = fullscreenManageHook
                               <+> myManagementHook
                               <+> namedScratchpadManageHook myScratchPads
        , handleEventHook    = myServerEventHook
                               <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
                               <+> dockStartupHook
                               <+> setDefaultCursor xC_left_ptr
                               >> addEWMHFullscreen
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColour
        , focusedBorderColor = myFocusColour
        , logHook            = dockEventLogHook
        }
    `additionalKeysP` keys
