# Xmonad profile manager for Gnome

More easily manageable xmonad profiles.
For now, it supports two distributions:
- Ubuntu
- Arch

## Installation

Requires:
- [Git](https://git-scm.com/) for cloning
- [GHCup](https://www.haskell.org/ghcup/)
- [Cabal](https://www.haskell.org/cabal/), preferably installed via GHCup

These should be installed for the manager to function.

Recommended:
- Gnome desktop environment

Procedure:
1. `git clone` this repository at suitable location to manage the profiles inside.
2. `cd xmonad-profiles` to get into the repo directory.
3. Run `./install.sh` to install `xmonad-manage`.
    * Likely requires granting executable permission.
    * Adds `xmonad-manage` executable `/opt/bin`. This might change in near future.
4. Run `xmonad-manage setup` to install required components.
5. Run `xmonad-manage install <profile-path>` to install the specified profile.
    * Currently supported profiles: `pulpmonad`, `xmonad-test`

This procedure automatically installs profiles in `/usr/share/xsessions/`,
which you can select from the login screen.

## XMonad-Manage

`xmonad-manage` is a tool to manage xmonad profiles.

- `xmonad-manage setup`: Installs common components.
- `xmonad-manage install <profile-path>`: Installs profile from `<profile-path>`.
- `xmonad-manage build <profile-id>`: Builds config for profile `<profile-id>`
- `xmonad-manage run <profile-id>`: Runs XMonad instance of profile `<profile-id>`

Run `xmonad-manage` to check further details.

## XMonad-Test

`xmonad-test` is a reference profile for `xmonad-manage`.
Consult its files to see how to make a profile.

## Notes
### Removal

NB: Manager removal method is not yet supported; this is due to `cabal` lacking uninstall/gc methods.

If you don't mind nuking the haskell stores, you can remove the `xmonad-profiles` directory and `$HOME/.ghcup`, `$HOME/.cabal` to do the cleanup.

You might also want to clean up `${XDG_DATA_DIR}/xmonad-manage`, which contains small data files.

NB: Individual profiles could be removed,
but the system dependencies are not deleted since you may have other packages dependent to it.
