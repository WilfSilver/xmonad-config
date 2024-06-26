module ScratchPad (
    myScratchPads,
    spawnScratchPad,
) where

import qualified Settings as S
import XMonad (
    X,
    className,
    title,
    (=?),
 )
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (
    NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
 )

-- TODO define constants for these

myScratchPads :: S.Config -> [NamedScratchpad]
myScratchPads c =
    [ NS
        "term"
        (S.terminal c ++ " -T scratchpad")
        (title =? "scratchpad")
        (customFloating $ W.RationalRect (3 / 5) (4 / 6) (1 / 5) (1 / 6))
    , NS
        "pavucontrol"
        "pavucontrol"
        (className =? "Pavucontrol")
        (customFloating $ W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4))
    ]

spawnScratchPad :: S.Config -> String -> X ()
spawnScratchPad c = namedScratchpadAction (myScratchPads c)
