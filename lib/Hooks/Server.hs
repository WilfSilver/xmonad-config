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

myXCommands :: X [(String, X ())]
myXCommands = return myCommands

myServerEventHook =
  serverModeEventHookCmd' myXCommands
    <+> serverModeEventHook' myXCommands
    <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
