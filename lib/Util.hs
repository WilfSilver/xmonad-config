module Util (
  currentWorkspace,
  getNonEmptyWorkspaces,
  getWindowTitle,
  visibleWorkspaces,
) where

import XMonad (
  Window,
  WorkspaceId,
  X,
  gets,
  windowset,
 )
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)

-- Gets the current workspace
currentWorkspace :: X WorkspaceId
currentWorkspace = do
  winset <- gets windowset
  return $ W.currentTag winset

-- Gets the workspaces that have a window on it (this is a getter because it needs an input)
getNonEmptyWorkspaces :: [W.Workspace WorkspaceId l a] -> [WorkspaceId]
getNonEmptyWorkspaces [] = []
getNonEmptyWorkspaces (x : xs) = do
  let stack = W.stack x
  case stack of
    Nothing -> getNonEmptyWorkspaces xs
    Just _ -> W.tag x : getNonEmptyWorkspaces xs

-- Returns windows title
getWindowTitle :: Window -> X String
getWindowTitle win = do
  namedWin <- getName win
  return $ show namedWin

-- Gets a list of the visible workspaces
visibleWorkspaces :: X [WorkspaceId]
visibleWorkspaces = do
  winset <- gets windowset
  return $ map (W.tag . W.workspace) (W.visible winset)
