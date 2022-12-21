-- xmonadctl to communicate with the xmonad config via XMonad.Hooks.ServerMode
-- This config was originally copied from https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-ServerMode.html but has been altered since

import           Control.Monad                  ( unless )
import           Graphics.X11.Xlib              ( allocaXEvent
                                                , clientMessage
                                                , defaultScreen
                                                , internAtom
                                                , openDisplay
                                                , rootWindow
                                                , sendEvent
                                                , structureNotifyMask
                                                , sync
                                                )
import           Graphics.X11.Xlib.Extras       ( currentTime
                                                , setClientMessageEvent
                                                , setEventType
                                                )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.IO                      ( hPutStrLn
                                                , isEOF
                                                , stderr
                                                )

main :: IO ()
main = parse True "XMONAD_COMMAND" =<< getArgs

parse :: Bool -> String -> [String] -> IO ()
parse input addr args = case args of
  ["--"] | input     -> repl addr
         | otherwise -> return ()
  ("--"        : xs) -> sendAll addr xs
  ("-a" : a    : xs) -> parse input a xs
  ("-h"        : _ ) -> showHelp
  ("--help"    : _ ) -> showHelp
  ("-?"        : _ ) -> showHelp
  (a@('-' : _) : _ ) -> hPutStrLn stderr ("Unknown option " ++ a)

  (x           : xs) -> sendCommand addr x >> parse False addr xs
  [] | input     -> showHelp
     | otherwise -> return ()

repl :: String -> IO ()
repl addr = do
  e <- isEOF
  unless e (do
    l <- getLine
    sendCommand addr l
    repl addr)

sendAll :: String -> [String] -> IO ()
sendAll addr ss = foldr (\a b -> sendCommand addr a >> b) (return ()) ss

sendCommand :: String -> String -> IO ()
sendCommand addr s = do
  d  <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a  <- internAtom d addr False
  m  <- internAtom d s False
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw a 32 m currentTime
    sendEvent d rw False structureNotifyMask e
    sync d False

showHelp :: IO ()
showHelp = do
  pn <- getProgName
  putStrLn
    ("Send commands to a running instance of xmonad. xmonad.hs must be configured with XMonad.Hooks.ServerMode to work.\n"
    ++ "-a atom name can be used at any point in the command line arguments to change which atom it is sending on.\n"
    ++ "If sent with '--' or only -a atom arguments, it will read commands from stdin.\n"
    ++ "Ex:\n"
    ++ pn
    ++ " cmd1 cmd2\n"
    ++ pn
    ++ " -a XMONAD_COMMAND cmd1 cmd2 cmd3 -a XMONAD_PRINT hello world\n"
    ++ pn
    ++ " -a XMONAD_PRINT # will read data from stdin.\n"
    ++ "The atom defaults to XMONAD_COMMAND."
    )
