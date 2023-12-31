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
- Bar: Eww (specifically my branch)
  - This currently writes to files in json format which can be then read by
    other programs to get the workspace names etc
- Application switcher: Rofi
- Terminal: Kitty
- Browser: Firefox
- Backgrounds: Nitrogen
- Compositor: picom
- Lockscreen: xsecurelock
- Notifications: Dunst
- Other applications mentioned in config:
  - Spotify
  - Discord
  - Signal
  - Gimp
  - Thunderbird

## Current plan

This is just a rough plan as I don't actively work on this unless its broken or
I want to lean about something

- Move more information to config files (maybe not json files... but probably
  going to be json)
- Improve the `xmonadctl` experience
  - I kinda want to add pipes so I don't have to write to files for the bar
    communication

## Install guide

This is for void linux but can be translated to work with arch

### Dependencies

Normally I install haskell through ghcup (definitely recommended for this
project) but you will need the following packages for stack to work properly:

```sh
sudo xbps-install \
  git gmp-devel iana-etc zlib gmp libffi libnuma 
```

On void linux, the `xmonad` does not come with a bunch of haskell dependencies,
so you can also install that (on arch probably just manually add the session
file etc)

I currently don't use void linux so I can't test this out

```sh
sudo xbps-install \
  libX11-devel \
  libXScrnSaver-devel \
  libXft-devel \
  libXinerama-devel \
  libXrandr-devel
```

You should then be able to run:

```
stack install
```

### Developing

If you are wanting HLS to work, the current GHC version is locked on 9.4.8 for
this project (stack will automatically download). But you will have to make
sure you have a version of HLS which works with this - currently the latest
should work but this could change so please check [this page](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)
