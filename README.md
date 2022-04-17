# Xmonad profile manager for Gnome

More easily manageable xmonad profiles on Gnome.

NB: Only works for the listed submodules.

## Installation

Requires:
- Gnome desktop environment (Some parts of it, e.g. `apt`)
- [Git](https://git-scm.com/) for cloning
- [GHCup](https://www.haskell.org/ghcup/)
- [Cabal](https://www.haskell.org/cabal/), preferably installed via GHCup

Procedure:
1. `git clone` this repository at suitable location to manage the profiles inside.
2. `cd xmonad-profiles` to get into the repo directory.
3. Run `install/Setup.hs . [profile-name]` to install required components.
  - Likely requires granting executable permission.
  - Can install multiple profiles.
  - Currently supported profiles: `pulpmonad`, `xmonad-test`

This procedure automatically installs profiles in `/usr/share/xsessions/`,
which you can select from login screen.

NB: Removal method is not yet supported; this is due to `cabal` lacking uninstall/gc methods.

If you don't mind nuking the haskell stores,
you can remove this directory and `$HOME/.ghcup`, `$HOME/.cabal` to do the cleanup.
