# Changelog
## master (0.8.1)
- Now, `xmonad-manage` detect presence of library packages and avoid re-installing them.
- Display configuration is applied on login, instead of on each setup.
  - Now it uses a temporary file to feed `xsettingsd`, instead of directly writing to its configuration.

## 0.8.0
- **! Required to run both `xmonad-manage setup` and re-installing your profile !**
- **Changed the install directory to `/usr/local`.**
  - `xmonad-manage` executable is copied into `/usr/local/bin`.
  - Shared configurations are copied into `/usr/local/etc`.
  - Data files are copied into `/usr/local/share`.
- Now `xmonad-manage` follows XDG Directory standard.
  - `xmonad-manage` logs are stored in `~/.local/state/xmonad-manage`.
  - `xmonad-manage` saves has been stored in `~/.local/share/xmonad-manage`.
  - User configurations are copied and stored in `~/.config/xmonad-manage`.
  - Respective profile data is stored in `~/.local/share/`, while cache is stored in `~/.cache`.
    Note that the cache contains the built executables from the profile.
- Logging has been improved a bit.
  - Now profile logs show ID at its heading.
- On startup, environments are reflected into systemd user environment.
- New command verb `log-output`: View log output from running `xmonad-manage run` on login.

## 0.7.3
- Removed update command, please use `./install.sh` instead.
- Install command can reinstall as well; Simply specify ID in place of the path.
- Logging headers on each output/error lines are removed.
- xmonad-manage is simple executable without systemd service again.
  - There were issues with inheriting the environment variables.
- Service installer does not expand `${Home}` now, use `%h` instead.
- Logs are redirected on `xmonad-manage run` to `./logs`.
  - On the beginning before redirection, look into session error directories like `~/.xsession-errors`.

## 0.7.2
- **! Recommends running `xmonad-manage setup` and re-installing your profile !**
- xmonad-manage is now installed in `/opt/bin`.
- xmonad-manage now runs as a systemd service.

## 0.7.1
- Active modules are now based on configuration in `config/active-modules.yaml`.
- X11 display configurations could be configured via `config/display-config.yaml`.

## 0.7.0
- Now depends on systemd; profiles operate as a systemd service.
- Introduced `reset-save` command, only use it when the save gets corrupted.
- (Changelogs in the previous versions have been lost)

## 0.5
- Added `remove` command, which removes specified profile.
- Fixed `install` not creating required directories.

## 0.4
(Changelog from this point is lost)
