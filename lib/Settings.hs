{-# LANGUAGE OverloadedStrings #-}

module Settings (
    ColourConfig,
    Workspaces,
    Config,
    StartUp,
    process,
    once,
    startup,
    norm,
    focus,
    current,
    visible,
    nonEmpty,
    layoutBg,
    workspaceBg,
    barBg,
    font,
    modMask,
    borderWidth,
    terminal,
    colours,
    workspaces,
    getWs,
    getConfigDir,
    getConfigFile,
    mySettings,
    titleLogFile,
    wsLogFile,
) where

import System.Environment (getEnv)
import XMonad (
    Dimension,
    KeyMask,
    WorkspaceId,
    mod1Mask,
    mod2Mask,
    mod3Mask,
    mod4Mask,
    mod5Mask,
 )

import Control.Monad (mzero)
import Data.Aeson (
    FromJSON,
    Value (Object, String),
    parseJSON,
    withScientific,
    (.:),
    (.:?),
 )
import Data.Text (unpack)
import Data.Yaml (ParseException, decodeFileEither)

type Workspaces = [WorkspaceId]

data StartUp = StartUp
    { process :: !String
    , once :: !(Maybe Bool)
    }
    deriving (Show)

instance FromJSON StartUp where
    parseJSON (Object v) =
        StartUp
            <$> v
                .: "process"
            <*> v
                .:? "once"
    parseJSON (String v) =
        return
            StartUp
                { process = unpack v
                , once = Just False
                }
    parseJSON _ = mzero

data ColourConfig = ColourConfig
    { norm :: !String -- Border color of normal windows
    , focus :: !String -- Border color of focused windows
    , current :: !String -- Colour of the current active workspace in the bar
    , visible :: !String -- Colour of the visible workspaces in the bar (e.g. workspaces on different screens)
    , nonEmpty :: !String -- Colour of the workspaces with windows open on them
    , layoutBg :: !String
    , workspaceBg :: !String
    , barBg :: !String
    }
    deriving (Show)

instance FromJSON ColourConfig where
    parseJSON (Object v) =
        ColourConfig
            <$> v .: "norm"
            <*> v .: "focus"
            <*> v .: "current"
            <*> v .: "visible"
            <*> v .: "nonEmpty"
            <*> v .: "layoutBg"
            <*> v .: "workspaceBg"
            <*> v .: "barBg"
    parseJSON _ = mzero

data Config = Config
    { font :: !String
    , modMask :: !KeyMask
    , terminal :: !String
    , borderWidth :: !Dimension
    , colours :: !ColourConfig
    , workspaces :: !Workspaces
    , startup :: ![StartUp]
    , titleLogFile :: !FilePath
    , wsLogFile :: !FilePath
    }
    deriving (Show)

toMasks 1 = return mod1Mask
toMasks 2 = return mod2Mask
toMasks 3 = return mod3Mask
toMasks 4 = return mod4Mask
toMasks 5 = return mod5Mask
toMasks _ = fail "Mod mask has to be a number between 0-5"

instance FromJSON Config where
    parseJSON (Object v) = do
        myModMask <- v .: "modMask"

        Config
            <$> v .: "font"
            <*> withScientific
                "KeyMask"
                toMasks
                myModMask
            <*> v .: "terminal"
            <*> v .: "borderWidth"
            <*> v .: "colours"
            <*> v .: "workspaces"
            <*> v .: "startup"
            <*> v .: "titleLogFile"
            <*> v .: "wsLogFile"
    parseJSON _ = mzero

getWs :: Int -> Config -> WorkspaceId
getWs i c = workspaces c !! i

getConfigDir :: IO String
getConfigDir = do
    path <- getEnv "HOME"
    return (path ++ "/.xmonad/config")

getConfigFile :: String -> IO String
getConfigFile file = do
    path <- getConfigDir
    return (path ++ "/" ++ file)

mySettings :: IO Config
mySettings = do
    configFile <- getConfigFile "settings.yaml"
    output <-
        decodeFileEither configFile :: IO (Either ParseException Config)
    case output of
        Left err -> do
            print err
            fail ("Invalid config file: " ++ show err)
        Right ps -> return ps
