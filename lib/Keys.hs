{-#LANGUAGE OverloadedStrings#-}
module Keys
  ( myKeys
  , myShortcuts
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

import           Control.Monad                  ( mzero )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON
                                                , Value(Object)
                                                , eitherDecode
                                                , parseJSON
                                                )
import qualified Data.ByteString.Lazy          as B
import           Data.Text                      ( Text
                                                , unpack
                                                )

import           Layouts                        ( toggleFullscreen )
import           ScratchPad                     ( spawnScratchPad )
import           Settings                       ( myTerminal )

data Shortcut = Shortcut
  { id          :: Text
  , description :: Text
  , binding     :: Text
  , command     :: Maybe Text
  }
  deriving Show

instance FromJSON Shortcut where
  parseJSON (Object v) =
    Shortcut
      <$> v
      .:  "id"
      <*> v
      .:  "description"
      <*> v
      .:  "binding"
      <*> v
      .:? "command"
  parseJSON _ = mzero

  -- TODO: Make it so it is dynamic to current user's directory or xmonad config
shortcutsConfigFile :: FilePath
shortcutsConfigFile = "/home/hugo/.xmonad/config/keys.json"

getShortcutsConfig :: IO B.ByteString
getShortcutsConfig = B.readFile shortcutsConfigFile

myShortcuts :: IO [Shortcut]
myShortcuts = do
  output <-
    (eitherDecode <$> getShortcutsConfig) :: IO (Either String [Shortcut])
  case output of
    Left  _  -> return []
    Right ps -> return ps


getKeyCommand :: Text -> X ()
getKeyCommand keyID | keyID == "xmonadRecompile"    = spawn "xmonad --recompile"
                    | keyID == "xmonadRestart"      = spawn "xmonad --restart"
                    | keyID == "xmonadQuit"         = io exitSuccess
                    | keyID == "closeCurrent"       = kill1
                    | keyID == "closeAllWs"         = killAll
                    | keyID == "toggleFloat" = sendMessage (T.Toggle "floats")
                    | keyID == "floatToTile" = withFocused $ windows . W.sink
                    | keyID == "allFloatToTile"     = sinkAll
                    | keyID == "moveToMasterWin"    = windows W.focusMaster
                    | keyID == "moveToNextWin"      = windows W.focusDown
                    | keyID == "moveToPrevWin"      = windows W.focusUp
                    | keyID == "swapFocusMasterWin" = windows W.swapMaster
                    | keyID == "swapFocusNextWin"   = windows W.swapDown
                    | keyID == "swapFocusPrevWin"   = windows W.swapUp
                    | keyID == "moveFocusMasterWin" = promote
                    | keyID == "rotateAllExMaster"  = rotSlavesDown
                    | keyID == "toggleFloat"        = rotAllDown
                    | keyID == "switchNextLayout"   = sendMessage NextLayout
                    | keyID == "arrange"            = sendMessage Arrange
                    | keyID == "deArrange"          = sendMessage DeArrange
                    | keyID == "fullscreen"         = toggleFullscreen
                    | keyID == "strutsToggle"       = sendMessage ToggleStruts
                    | keyID == "shrinkWinHorz"      = sendMessage Shrink
                    | keyID == "expWinHorz"         = sendMessage Expand
                    | keyID == "shrinkWinVert"      = sendMessage MirrorShrink
                    | keyID == "expWinVert"         = sendMessage MirrorExpand
                    | keyID == "switchNextMonitor"  = nextScreen
                    | keyID == "switchPrevMonitor"  = prevScreen
                    | keyID == "switchNextWs"       = nextWS
                    | keyID == "switchPrevWs"       = prevWS
                    | keyID == "term"               = spawn myTerminal
                    | keyID == "termScratch"        = spawnScratchPad "term"
                    | keyID == "volumeScratch" = spawnScratchPad "pavucontrol"
                    | otherwise                     = return ()


extractKeys :: Shortcut -> (String, X ())
extractKeys key =
  ( unpack $ binding key
  , maybe (getKeyCommand $ Keys.id key) (spawn . unpack) (command key)
  )

myKeys :: IO [(String, X ())]
myKeys = do
  keys <- myShortcuts
  return $ map extractKeys keys
