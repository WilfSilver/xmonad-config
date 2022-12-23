{-#LANGUAGE OverloadedStrings#-}
module Keys
  ( myKeys
  , myShortcuts
  ) where

import           XMonad                         ( X
                                                , spawn
                                                )

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

import           Commands                       ( getCommand )

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

extractKeys :: Shortcut -> (String, X ())
extractKeys key =
  ( unpack $ binding key
  , maybe (getCommand $ Keys.id key) (spawn . unpack) (command key)
  )

myKeys :: IO [(String, X ())]
myKeys = do
  keys <- myShortcuts
  return $ map extractKeys keys
