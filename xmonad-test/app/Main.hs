module Main where

import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

main :: IO ()
main = do
  spawn "feh --bg-scale /home/abastro/Pictures/Universe/Andromeda_Galaxy.jpg"
  xmonad . ewmhFullscreen $
    desktopConfig
      { modMask = mod4Mask
      , startupHook =
          startupHook desktopConfig <> do
            setWMName "LG3D"
            gnomeRegister -- Registers xmonad with gnome
      , terminal = "gnome-terminal"
      , manageHook = isDialog --> doFloat
      }
