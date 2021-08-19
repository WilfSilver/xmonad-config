module ScratchPad
  ( myScratchPads
  , spawnScratchPad
  ) where

import           Settings                       ( myTerminal )
import           XMonad                         ( (=?)
                                                , X
                                                , className
                                                , title
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad    ( NamedScratchpad(NS)
                                                , customFloating
                                                , namedScratchpadAction
                                                )

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [
    -- NS "taskwarrior" "urxvtc -name taskwarrior -e ~/bin/tw" (resource =? "taskwarrior")
    --     (customFloating $ W.RationalRect (2/6) (2/6) (2/6) (2/6))
    NS "term"
       (myTerminal ++ " -T scratchpad")
       (title =? "scratchpad")
       (customFloating $ W.RationalRect (3 / 5) (4 / 6) (1 / 5) (1 / 6))
  , NS "pavucontrol"
       "pavucontrol"
       (className =? "Pavucontrol")
       (customFloating $ W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4))
  ]

spawnScratchPad :: String -> X ()
spawnScratchPad = namedScratchpadAction myScratchPads
