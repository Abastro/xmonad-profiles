# Xmonad profile manager for Gnome

More easily manageable xmonad profiles on Ubuntu.

NB: Only works for the listed submodules.

## Installation

Requires:
- Ubuntu with Gnome desktop environment
- [Git](https://git-scm.com/) for cloning
- [GHCup](https://www.haskell.org/ghcup/)
- [Cabal](https://www.haskell.org/cabal/), preferably installed via GHCup

Procedure:
1. `git clone` this repository at suitable location to manage the profiles inside.
2. `cd xmonad-profiles` to get into the repo directory.
3. Run `./install.sh` to install `xmonad-manage`
    * Likely requires granting executable permission.
    * Adds `xmonad-manage` executable `$HOME/.cabal/bin`. This might change in near future.
4. Run `xmonad-manage install` to install required components.
5. Run `xmonad-manage install <profile-path>` to install the specified profile.
    * Currently supported profiles: `pulpmonad`, `xmonad-test`

This procedure automatically installs profiles in `/usr/share/xsessions/`,
which you can select from login screen.

## XMonad-Manage

`xmonad-manage` is a tool to manage xmonad profiles.

- `xmonad-manage install`: Installs common components.
- `xmonad-manage install <profile-path>`: Installs profile from `<profile-path>`.
- `xmonad-manage build <profile-id>`: Manually builds config for profile `<profile-id>`
- `xmonad-manage run <profile-id>`: Runs XMonad instance of profile `<profile-id>`

## Notes

NB: Removal method is not yet supported; this is due to `cabal` lacking uninstall/gc methods.

If you don't mind nuking the haskell stores, you can remove the `xmonad-profiles` directory and `$HOME/.ghcup`, `$HOME/.cabal` to do the cleanup.
- Added: Also need to clean up `XDG_DATA_DIR/xmonad-manage`, but this folder contains a single small text data file.
