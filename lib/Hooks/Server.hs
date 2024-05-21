module Hooks.Server (
    myServerEventHook,
) where

-- Extension to XMonad.Actions.Commands to have custom commands

import XMonad (
    X,
    io,
    (<+>),
 )
import XMonad.Hooks.ServerMode (
    serverModeEventHook',
    serverModeEventHookCmd',
    serverModeEventHookF,
 )

import Commands (myCommands)
import qualified Settings as S

myXCommands :: S.Config -> X [(String, X ())]
myXCommands = return . myCommands

myServerEventHook c =
    serverModeEventHookCmd' (myXCommands c)
        <+> serverModeEventHook' (myXCommands c)
        <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
