module Layouts
  ( mySpacing
  , tall
  , magnify
  , monocle
  , floats
  , tabs
  , toggleFullscreen
  , myLayoutHook
  ) where

import           XMonad                         ( Default(def)
                                                , Full(Full)
                                                , LayoutClass(description)
                                                , Mirror(Mirror)
                                                , X
                                                , sendMessage
                                                , (|||)
                                                )
import           XMonad.Layout.Decoration       ( Decoration
                                                , DefaultShrinker
                                                )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.LimitWindows     ( LimitWindows
                                                , limitWindows
                                                )
import           XMonad.Layout.Magnifier        ( Magnifier
                                                , magnifier
                                                )
import           XMonad.Layout.MultiToggle      ( (??)
                                                , EOT(EOT)
                                                , Toggle(..)
                                                , mkToggle
                                                )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers
                                                  ( NBFULL
                                                  , NOBORDERS
                                                  )
                                                )
import           XMonad.Layout.NoBorders        ( noBorders )
import           XMonad.Layout.Renamed          ( Rename(Replace)
                                                , renamed
                                                )
import           XMonad.Layout.ResizableTile    ( ResizableTall(ResizableTall) )
import           XMonad.Layout.Simplest         ( Simplest )
import           XMonad.Layout.SimplestFloat    ( SimplestFloat
                                                , simplestFloat
                                                )
import           XMonad.Layout.Spacing          ( Border(Border)
                                                , Spacing
                                                , spacingRaw
                                                )
import           XMonad.Layout.Tabbed           ( TabbedDecoration
                                                , Theme
                                                  ( activeBorderColor
                                                  , activeColor
                                                  , activeTextColor
                                                  , fontName
                                                  , inactiveBorderColor
                                                  , inactiveColor
                                                  , inactiveTextColor
                                                  )
                                                , shrinkText
                                                , tabbed
                                                )
import qualified XMonad.Layout.ToggleLayouts   as T
                                                ( toggleLayouts )
import           XMonad.Layout.WindowArranger   ( WindowArranger
                                                , windowArrange
                                                )

import           XMonad.Actions.MouseResize     ( mouseResize )
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..)
                                                , avoidStruts
                                                )
import           XMonad.Layout.PerWorkspace     ( onWorkspace
                                                , onWorkspaces
                                                )

import           PerScreen                      ( PerScreen
                                                , ifWider
                                                )
import           Settings                       ( myFont
                                                , myWorkspaces
                                                )

mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i i i) True (Border 0 i i i) True

tall
  :: ModifiedLayout
       Rename
       (ModifiedLayout LimitWindows (ModifiedLayout Spacing ResizableTall))
       a
tall =
  renamed [Replace "\xf0ce"] $ limitWindows 12 $ mySpacing 4 $ ResizableTall
    1
    (3 / 100)
    (1 / 2)
    []

magnify
  :: Eq a
  => ModifiedLayout
       Rename
       ( ModifiedLayout
           Magnifier
           ( ModifiedLayout
               LimitWindows
               (ModifiedLayout Spacing ResizableTall)
           )
       )
       a
magnify =
  renamed [Replace "\xf00e"]
    $ magnifier
    $ limitWindows 12
    $ mySpacing 0
    $ ResizableTall 1 (3 / 100) (1 / 2) []

monocle :: Eq a => ModifiedLayout Rename (ModifiedLayout LimitWindows Full) a
monocle = renamed [Replace "\x1f9d0"] $ limitWindows 20 Full

floats
  :: Eq a
  => ModifiedLayout
       Rename
       ( ModifiedLayout
           LimitWindows
           (ModifiedLayout WindowArranger SimplestFloat)
       )
       a
floats = renamed [Replace "\xf21a"] $ limitWindows 20 simplestFloat

tabs
  :: Eq a
  => ModifiedLayout
       Rename
       ( ModifiedLayout
           (Decoration TabbedDecoration DefaultShrinker)
           Simplest
       )
       a
tabs = renamed [Replace "\xf03c"] $ tabbed shrinkText myTabConfig
 where
  myTabConfig = def { fontName            = myFont
                    , activeColor         = "#282c34"
                    , inactiveColor       = "#3e445e"
                    , activeBorderColor   = "#282c34"
                    , inactiveBorderColor = "#282c34"
                    , activeTextColor     = "#ffffff"
                    , inactiveTextColor   = "#d0d0d0"
                    }

toggleFullscreen :: X ()
toggleFullscreen = sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts

myLayoutHook =
  avoidStruts
    $ mouseResize
    $ windowArrange
    $ T.toggleLayouts floats
    $ onWorkspaces [head myWorkspaces, myWorkspaces !! 4]
                   (noBorders monocle ||| getPerScreenLayout tall)
    $ onWorkspace
        (myWorkspaces !! 1)
        (   getPerScreenLayout (noBorders magnify)
        ||| getPerScreenLayout (noBorders tall)
        ||| noBorders monocle
        )
    $ onWorkspace (myWorkspaces !! 7) floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
 where
  myDefaultLayout =
    getPerScreenLayout tall
      ||| getPerScreenLayout magnify
      ||| noBorders monocle
      ||| getPerScreenLayout floats
      ||| noBorders tabs

getPerScreenLayout
  :: LayoutClass l a => l a -> PerScreen l (ModifiedLayout Rename (Mirror l)) a
getPerScreenLayout l = ifWider l $ renamed [Replace $ description l] $ Mirror l
