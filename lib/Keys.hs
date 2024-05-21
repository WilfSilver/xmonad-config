{-# LANGUAGE OverloadedStrings #-}

module Keys (
    myKeys,
    myShortcuts,
) where

import XMonad (
    X,
    spawn,
 )

import Control.Monad (mzero)
import Data.Aeson (
    FromJSON,
    Value (Object),
    parseJSON,
    (.:),
    (.:?),
 )
import Data.Text (
    Text,
    unpack,
 )

import Commands (getCommand)
import Data.Yaml (ParseException, decodeFileEither)
import qualified Settings as S

data Shortcut = Shortcut
    { id :: !Text
    , description :: !Text
    , binding :: !Text
    , command :: !(Maybe Text)
    }
    deriving (Show)

instance FromJSON Shortcut where
    parseJSON (Object v) =
        Shortcut
            <$> v .: "id"
            <*> v .: "description"
            <*> v .: "binding"
            <*> v .:? "command"
    parseJSON _ = mzero

shortcutsConfigFile :: IO FilePath
shortcutsConfigFile = S.getConfigFile "keys.yaml"

myShortcuts :: IO [Shortcut]
myShortcuts = do
    configFile <- shortcutsConfigFile
    output <-
        decodeFileEither configFile :: IO (Either ParseException [Shortcut])
    case output of
        Left _ -> return []
        Right ps -> return ps

extractKeys :: S.Config -> Shortcut -> (String, X ())
extractKeys c key =
    ( unpack $ binding key
    , maybe (getCommand c $ Keys.id key) (spawn . unpack) (command key)
    )

myKeys :: S.Config -> IO [(String, X ())]
myKeys c = map (extractKeys c) <$> myShortcuts
