-- My XMonad configuration

-- Base
import XMonad (
    Atom,
    Default (def),
    X,
    XConfig (
        borderWidth,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal,
        workspaces
    ),
    asks,
    changeProperty32,
    getAtom,
    getWindowProperty32,
    liftIO,
    propModeAppend,
    theRoot,
    withDisplay,
    xmonad,
    (<+>),
 )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Layout.Fullscreen (
    fullscreenEventHook,
    fullscreenManageHook,
    fullscreenSupport,
 )
import XMonad.Util.Cursor (
    setDefaultCursor,
    xC_left_ptr,
 )
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)

import Control.Monad (
    join,
    unless,
 )
import Data.Maybe (maybeToList)

import Hooks (
    myManagementHook,
    myStartupHook,
 )
import Hooks.Dock (
    dockEventLogHook,
    dockStartupHook,
 )
import Hooks.Layout (myLayoutHook)
import Hooks.Server (myServerEventHook)

import Keys (myKeys)

import ScratchPad (myScratchPads)

import Settings (mySettings)
import qualified Settings as S

-- (myBorderWidth,myFocusColour,myModMask,myNormColour,myTerminal,myWorkspaces,)

-- Stolen from https://github.com/evanjs/gentoo-dotfiles/commit/cbf78364ea60e62466594340090d8e99200e8e08 (thanks)

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
    root <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    atom <- getAtom "ATOM"
    liftIO $ do
        sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED root
        unless (fromIntegral x `elem` sup) $
            changeProperty32 dpy root a_NET_SUPPORTED atom propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

main :: IO ()
main = do
    myConfig <- mySettings
    let myColours = S.colours myConfig
    keys <- myKeys myConfig
    xmonad $
        fullscreenSupport $
            docks $
                ewmh $
                    def
                        { manageHook =
                            fullscreenManageHook
                                <+> myManagementHook myConfig
                                <+> namedScratchpadManageHook (myScratchPads myConfig)
                        , handleEventHook =
                            myServerEventHook myConfig
                                <+> fullscreenEventHook
                        , modMask = S.modMask myConfig
                        , terminal = S.terminal myConfig
                        , startupHook =
                            myStartupHook myConfig
                                <+> dockStartupHook
                                <+> setDefaultCursor xC_left_ptr
                                >> addEWMHFullscreen
                        , layoutHook = myLayoutHook myConfig
                        , workspaces = S.workspaces myConfig
                        , borderWidth = S.borderWidth myConfig
                        , normalBorderColor = S.norm myColours
                        , focusedBorderColor = S.focus myColours
                        , logHook = dockEventLogHook myConfig
                        }
                        `additionalKeysP` keys
