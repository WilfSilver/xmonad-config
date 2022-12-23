module Layout.Util
  ( toggleFullscreen
  ) where

import           XMonad                         ( X
                                                , sendMessage
                                                )
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..) )
import           XMonad.Layout.MultiToggle      ( Toggle(..) )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers(NBFULL) )

toggleFullscreen :: X ()
toggleFullscreen = sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts

