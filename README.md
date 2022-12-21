# My XMonad config

This is my xmonad config, I use this same config on a laptop (with tablet mode) and desktop (with 2 screens) so it is designed to be more dynamic than some others - however this is very unstable just because I am still exporing and learning new things.

I mainly use this to explore different things you can do with xmonad and cool stuff you can do as well as learning Haskell.

## Cool features

- `xmonadctl` is included with custom commands found in `Server.hs`
- The config is split over modules (Not well but still) to make it easier to read and understand
- Generalised way communication between your dock and xmonad so its not limited to one. (However you may have to have a script to convert it to the different formats)
- ScratchPads :)
- Autorotation: for screens that have a larger height than width, the layout auto rotates to accommodate the change.
- key bindings are stored in a separate json file so you don't have to recompile and restart the server when you change a key binding

## My current setup

- Font: FiraCode / FontAwesome
- Bar: Polybar (however currently testing to switch to eww)
- Application switcher: Rofi
- Terminal: Terminator
- Browser: Waterfox
- Backgrounds: Nitrogen
- Compositor: picom
- Lockscreen: betterlockscreen
- Notifications: Dunst
- Other applications mentioned in config:
  - Spotify
  - Discord
  - Signal
  - Gimp
  - Thunderbird
  - htop

## Current plan

- Move more information to config files (maybe not json files...)
- Improve the `xmonadctl` experience
- Tidy up more
- Better ScratchPads?
- Comment stuff like a proper haskell project
- Clean up the `my-xmonad.cabal`

## Install guide

### Arch Linux

### Void Linux

I currently don't use void linux so I can't test this out

sudo xbps-install \
  libX11-devel \
  libXScrnSaver-devel \
  libXft-devel \
  libXinerama-devel \
  libXrandr-devel

sudo xbps-install stack cabal-install

cabal install --lib xmonad xmonad-contrib X11
cabal install xmonad

add $HOME/.cabal/bin to PATH
