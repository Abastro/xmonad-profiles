# Changelog
## master
- Removed update command, please use `./install.sh` instead.
- Logging headers on each output/error lines are removed.
- Service installer does not expand `${Home}` now, use `%h` instead.
- xmonad-manage is simple executable without systemd service again.
  - There were issues with inheriting the environment variables.
- Logs are redirected on `xmonad-manage run` to `./logs`.

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
