# xmonad

## Install guide

### Void Linux

sudo xbps-install \
  libX11-devel \
  libXScrnSaver-devel \
  libXft-devel \
  libXinerama-devel \
  libXrandr-devel

sudo xbps-install stack cabal-install polybar

cabal install --lib xmonad xmonad-contrib X11
cabal install xmonad

add $HOME/.cabal/bin to PATH
